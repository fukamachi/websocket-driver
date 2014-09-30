(in-package :cl-user)
(defpackage websocket-driver.driver.hybi
  (:use :cl
        :annot.class
        :event-emitter
        :websocket-driver.driver.base
        :websocket-driver.events
        :websocket-driver.error)
  (:import-from :websocket-driver.header
                :finalize-headers)
  (:import-from :websocket-driver.socket
                :write-to-socket)
  (:import-from :fast-io
                :with-fast-output
                :with-fast-input
                :fast-write-byte
                :fast-write-sequence
                :fast-read-byte
                :fast-read-sequence
                :make-output-buffer
                :output-buffer-len
                :finish-output-buffer)
  (:import-from :cl-async-future
                :make-future
                :finish)
  (:import-from :ironclad
                :digest-sequence
                :ascii-string-to-byte-array)
  (:import-from :base64
                :usb8-array-to-base64-string)
  (:import-from :babel
                :string-to-octets
                :octets-to-string
                :character-decoding-error)
  (:import-from :alexandria
                :define-constant
                :when-let
                :plist-hash-table
                :hash-table-values))
(in-package :websocket-driver.driver.hybi)

(syntax:use-syntax :annot)

(defconstant +byte+    #b11111111)
(defconstant +fin+     #b10000000)
(defconstant +mask+    #b10000000)
(defconstant +rsv1+    #b01000000)
(defconstant +rsv2+    #b00100000)
(defconstant +rsv3+    #b00010000)
(defconstant +opcode+  #b00001111)
(defconstant +length+  #b01111111)

(define-constant +guid+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test 'equal)

(defconstant +min-reserved-error+ 3000)
(defconstant +max-reserved-error+ 4999)

(defparameter *opcodes-map*
  (plist-hash-table '(:continuation 0
                      :text         1
                      :binary       2
                      :close        8
                      :ping         9
                      :pong        10)
                    :test 'eq))

(defparameter *opcodes*
  (hash-table-values *opcodes-map*))

(defparameter *fragmented-opcodes*
  (mapcar (lambda (name)
            (gethash name *opcodes-map*))
          '(:continuation :text :binary)))

(defparameter *opening-opcodes*
  (mapcar (lambda (name)
            (gethash name *opcodes-map*))
          '(:text :binary)))

(defun opcode (name)
  (or (gethash name *opcodes-map*)
      (error "Unknown opcode name: ~S" name)))

@export
@export-accessors
(defclass hybi (driver)
  ((env :initarg :env
        :accessor env)
   (require-masking :initarg :require-masking
                    :initform nil
                    :accessor require-masking)
   (masking :initarg :masking
            :initform nil
            :accessor masking)

   (protocol :initform nil
             :accessor protocol)
   (mode :initform nil
         :accessor mode)
   (buffer :initform (fast-io::make-output-buffer)
           :accessor buffer)
   (ping-callbacks :initform (make-hash-table :test 'equalp)
                   :accessor ping-callbacks)))

(defmethod initialize-instance :after ((driver hybi) &key)
  (when (slot-boundp driver 'env)
    (let ((protocols (protocols driver))
          (env-protocols (getf (env driver)
                               :http-sec-websocket-protocol)))
      (when (stringp env-protocols)
        (setq env-protocols (ppcre:split "\\s*,\\s*" env-protocols)))
      (setf (protocol driver)
            (find (lambda (proto)
                    (member proto protocols :test #'string=))
                  env-protocols)))))

(defmethod version ((driver hybi))
  (format nil "hybi-~A"
          (getf (env driver)
                :http-sec-websocket-version)))

(defmethod send-text ((driver hybi) message)
  (send driver message :type :text))

(defmethod send-binary ((driver hybi) message)
  (send driver message :type :binary))

(defmethod send-ping ((driver hybi) &optional message callback)
  (unless message
    (setq message (make-array 0 :element-type '(unsigned-byte 8))))
  (when callback
    (setf (gethash message (ping-callbacks driver))
          callback))
  (send driver message :type :ping))

(defmethod close-connection ((driver hybi) &optional (reason "") (code (error-code :normal-closure)))
  (case (ready-state driver)
    (:connecting
     (setf (ready-state driver) :closed)
     (emit :close driver (make-close-event :code code :reason reason))
     T)
    (:open
     (send driver reason :type :close :code code)
     (setf (ready-state driver) :closing)
     T)
    (otherwise nil)))

(defun mask-message (data mask-keys)
  (check-type data (vector (unsigned-byte 8)))
  (let ((len (length data))
        (last (last mask-keys)))
    (rplacd last mask-keys)
    (do ((i 0 (1+ i))
         (next-mask mask-keys (cdr next-mask)))
        ((= len i)
         (rplacd last nil)
         data)
      (setf (aref data i)
            (logxor (aref data i) (car next-mask))))))

(defmethod send ((driver hybi) data &key type code)
  (when (eq (ready-state driver) :connecting)
    (return-from send
      (enqueue driver (list data type code))))

  (unless (eq (ready-state driver) :open)
    (return-from send nil))

  (unless type
    (setq type (if (stringp data) :text :binary)))

  (when (stringp data)
    (setq data (string-to-octets data :encoding :utf-8)))

  (let* ((opcode (opcode type))
         (insert (if code 2 0))
         (length (+ (length data) insert))
         (masked (if (masking driver)
                     +mask+
                     0))
         (frame
           (with-fast-output (frame :vector)
             (fast-write-byte (logxor +fin+ opcode) frame)
             (cond
               ((<= length 125)
                (fast-write-byte (logxor masked length) frame))
               ((<= length 65535)
                (fast-write-byte (logxor masked 126) frame)
                (fast-write-byte (logand (ash length -8) +byte+) frame)
                (fast-write-byte (logand length +byte+) frame))
               (T
                (fast-write-byte (logxor masked 127) frame)
                (fast-write-byte (logand (ash length -56) +byte+) frame)
                (fast-write-byte (logand (ash length -48) +byte+) frame)
                (fast-write-byte (logand (ash length -40) +byte+) frame)
                (fast-write-byte (logand (ash length -32) +byte+) frame)
                (fast-write-byte (logand (ash length -24) +byte+) frame)
                (fast-write-byte (logand (ash length -16) +byte+) frame)
                (fast-write-byte (logand (ash length -8) +byte+) frame)
                (fast-write-byte (logand length +byte+) frame)))

             (when code
               (setq data
                     (concatenate '(vector (unsigned-byte 8))
                                  (list (logand (ash code -8) +byte+)
                                        (logand code +byte+))
                                  data)))

             (when (masking driver)
               (let ((mask-keys
                       (loop :repeat 4
                             :for key := (random 256)
                             :do (fast-write-byte key frame)
                             :collect key)))
                 (setq data (mask-message data mask-keys))))

             (fast-write-sequence data frame)))
         (future (asf:make-future)))
    (write-to-socket (socket driver) frame
                     :callback
                     (lambda ()
                       (asf:finish future)))
    future))

@export
(defun generate-accept (key)
  (base64:usb8-array-to-base64-string
   (ironclad:digest-sequence :sha1
                             (ironclad:ascii-string-to-byte-array
                              (concatenate 'string key +guid+)))))

(defmethod handshake-response ((driver hybi))
  (let ((sec-key (getf (env driver)
                       :http-sec-websocket-key)))
    (unless (stringp sec-key)
      (return-from handshake-response
        (make-array 0 :element-type '(unsigned-byte 8))))

    (with-fast-output (buffer :vector)
      (fast-write-sequence
       #.(string-to-octets
          (with-output-to-string (s)
            (format s "HTTP/1.1 101 Switching Protocols~C~C" #\Return #\Linefeed)
            (format s "Upgrade: websocket~C~C" #\Return #\Linefeed)
            (format s "Connection: Upgrade~C~C" #\Return #\Linefeed)
            (format s "Sec-WebSocket-Accept: ")))
       buffer)
      (fast-write-sequence (string-to-octets (generate-accept sec-key)) buffer)
      (fast-write-sequence #.(string-to-octets (format nil "~C~C" #\Return #\Linefeed))
                           buffer)

      (when-let (protocol (protocol driver))
        (fast-write-sequence (string-to-octets
                              (format nil "Sec-WebSocket-Protocol: ~A~C~C"
                                      protocol
                                      #\Return #\Linefeed)
                              :encoding :utf-8)
                             buffer))

      (fast-write-sequence (finalize-headers (headers driver))
                           buffer)

      (fast-write-sequence #.(string-to-octets (format nil "~C~C" #\Return #\Linefeed))
                           buffer))))

(defmethod parse ((driver hybi) data)
  (let ((final nil)
        (opcode nil)
        (length nil)
        (length-size nil)
        (masked nil)
        (mask nil))
    (labels ((check-frame-length ()
               (unless (<= (+ (fast-io::output-buffer-len (buffer driver))
                              length)
                           (max-length driver))
                 (error 'too-large))
               T)
             (parse-opcode (byte)
               (let ((rsvs (mapcar (lambda (rsv)
                                     (= (logand byte rsv) rsv))
                                   (list +rsv1+
                                         +rsv2+
                                         +rsv3+))))
                 (when (some (complement #'null) rsvs)
                   (error 'protocol-error
                          :format-control "One or more reserved bits are on: reserved1 = ~A, reserved2 = ~A, reserved3 = ~A"
                          :format-arguments (list
                                             (if (nth 0 rsvs) 1 0)
                                             (if (nth 1 rsvs) 1 0)
                                             (if (nth 2 rsvs) 1 0)))))

               (setq final (= (logand byte +fin+) +fin+)
                     opcode (logand byte +opcode+))

               (unless (find opcode *opcodes* :test #'=)
                 (error 'protocol-error
                        :format-control "Unrecognized frame opcode: ~A"
                        :format-arguments (list opcode)))

               (unless (or final
                           (find opcode *fragmented-opcodes* :test #'=))
                 (error 'protocol-error
                        :format-control "Received fragmented control frame: opcode = ~A"
                        :format-arguments (list opcode)))

               (when (and (mode driver)
                          (find opcode *opening-opcodes* :test #'=))
                 (error 'protocol-error
                        :format-control "Received new data frame but previous continuous frame is unfinished"))

               (setf (stage driver) 1))
             (parse-length (byte)
               (setq masked (= (logand byte +mask+) +mask+))
               (when (and (require-masking driver)
                          (not masked))
                 (error 'unacceptable))

               (setq length (logand byte +length+))

               (cond
                 ((<= 0 length 125)
                  (check-frame-length)
                  (setf (stage driver) (if masked 3 4)))
                 (T
                  (setq length-size (if (= length 126) 2 8))
                  (setf (stage driver) 2))))
             (parse-extended-length (buffer)
               (setq length (length buffer))

               (unless (or (find opcode *fragmented-opcodes* :test #'=)
                           (<= length 125))
                 (error 'protocol-error
                        :format-control "Received control frame having too long payload: ~A"
                        :format-arguments (list length)))

               (check-frame-length)

               (setf (stage driver) (if masked 3 4)))
             (emit-frame (buffer)
               (let ((payload (if masked
                                  (mask-message buffer mask)
                                  buffer))
                     (finalp final)
                     (opc opcode))
                 (setq final nil
                       opcode nil
                       length nil
                       length-size nil
                       masked nil
                       mask nil)

                 (cond
                   ((= opc (opcode :continuation))
                    (unless (mode driver)
                      (error 'protocol-error
                             :format-control "Received unexpected continuation frame"))
                    (fast-write-sequence payload (buffer driver))
                    (when finalp
                      (let ((message (fast-io::finish-output-buffer (buffer driver)))
                            (mode (mode driver)))
                        (setf (buffer driver) (fast-io::make-output-buffer)
                              (mode driver) nil)
                        (handler-case
                            (emit :message
                                  driver
                                  (make-message-event :data (if (eq mode :text)
                                                                (octets-to-string message :encoding :utf-8)
                                                                message)))
                          (babel:character-decoding-error ()
                            (error 'encoding-error))))))
                   ((= opc (opcode :text))
                    (if finalp
                        (handler-case
                            (emit :message driver
                                  (make-message-event :data (octets-to-string payload :encoding :utf-8)))
                          (babel:character-decoding-error ()
                            (error 'encoding-error)))
                        (progn
                          (setf (mode driver) :text)
                          (fast-write-sequence payload (buffer driver)))))
                   ((= opc (opcode :binary))
                    (if finalp
                        (emit :message driver (make-message-event :data payload))
                        (progn
                          (setf (mode driver) :binary)
                          (fast-write-sequence payload (buffer driver)))))
                   ((= opc (opcode :close))
                    (let ((code (if (<= 2 (length payload))
                                    (* 256 (aref payload 0) (aref payload 1))
                                    nil)))
                      (unless (or (zerop (length payload))
                                  (and code
                                       (<= +min-reserved-error+ code +max-reserved-error+))
                                  (valid-error-code-p code))
                        (setq code (error-code :protocol-error)))

                      (let ((reason (if (<= 2 (length payload))
                                        (subseq payload 2)
                                        (make-array 0 :element-type '(unsigned-byte 8)))))
                        (when (< (length payload) 125)
                          (setq code (error-code :protocol-error)))
                        (shutdown code reason))))
                   ((= opc (opcode :ping))
                    (send driver payload :type :pong))
                   ((= opc (opcode :pong))
                    (let ((callback (gethash payload (ping-callbacks driver))))
                      (when callback
                        (remhash payload (ping-callbacks driver))
                        (funcall callback)))))))
             (shutdown (code reason)
               (send driver reason :type :close :code code)
               (setf (ready-state driver) :closed)
               (setf (stage driver) 5)
               (emit :close driver (make-close-event :code code :reason reason))))
      (with-fast-input (data-buffer data)
        (let ((buffer t))
          (flet ((read-seq-with-length (length)
                   (let* ((buf (make-array length :element-type '(unsigned-byte 8)))
                          (end (fast-read-sequence buf data-buffer)))
                     (if (= length end)
                         buf
                         nil))))
            (handler-case
                (do () ((null buffer))
                  (case (stage driver)
                    (0
                     (setq buffer (read-seq-with-length 1))
                     (when buffer
                       (parse-opcode (aref buffer 0))))
                    (1
                     (setq buffer (read-seq-with-length 1))
                     (when buffer
                       (parse-length (aref buffer 0))))
                    (2
                     (setq buffer (read-seq-with-length length-size))
                     (when buffer
                       (parse-extended-length buffer)))
                    (3
                     (setq buffer (read-seq-with-length 4))
                     (when buffer
                       (setq mask (coerce buffer 'list))
                       (setf (stage driver) 4)))
                    (4
                     (setq buffer (read-seq-with-length length))
                     (when buffer
                       (emit-frame buffer)
                       (setf (stage driver) 0)))
                    (otherwise
                     (setq buffer nil))))
              (protocol-error (e)
                (emit :error driver e)
                (shutdown (error-code e) (princ-to-string e))))))))))
