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
  (:import-from :fast-websocket
                :make-ws
                :ws-mask
                :ws-stage
                :make-parser
                :compose-frame)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-sequence)
  (:import-from :blackbird
                :with-promise)
  (:import-from :ironclad
                :digest-sequence
                :ascii-string-to-byte-array)
  (:import-from :base64
                :usb8-array-to-base64-string)
  (:import-from :babel
                :string-to-octets)
  (:import-from :alexandria
                :define-constant
                :when-let))
(in-package :websocket-driver.driver.hybi)

(syntax:use-syntax :annot)

(define-constant +guid+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test 'equal)

@export
@export-accessors
(defclass hybi (driver)
  ((env :initarg :env
        :accessor env)
   (require-masking :initarg :require-masking
                    :initform nil
                    :accessor require-masking)

   (protocol :initform nil
             :accessor protocol)
   (ping-callbacks :initform (make-hash-table :test 'equalp)
                   :accessor ping-callbacks)
   (ws :initform (fast-websocket:make-ws))
   (parser :accessor parser)))

(defmethod masking ((driver hybi))
  (fast-websocket:ws-mask (slot-value driver 'ws)))

(defmethod initialize-instance :after ((driver hybi) &key)
  (when (slot-boundp driver 'env)
    (let ((protocols (protocols driver))
          (env-protocols (gethash "sec-websocket-protocol"
                                  (getf (env driver) :headers))))
      (when (stringp env-protocols)
        (setq env-protocols (ppcre:split "\\s*,\\s*" env-protocols)))
      (setf (protocol driver)
            (find (lambda (proto)
                    (member proto protocols :test #'string=))
                  env-protocols))))
  (setf (parser driver)
        (let ((ws (slot-value driver 'ws)))
          (fast-websocket:make-parser
           ws
           :require-masking (require-masking driver)
           :max-length (max-length driver)
           :message-callback
           (lambda (message)
             (emit :message
                   driver
                   (make-message-event :data message)))
           :pong-callback
           (lambda (payload)
             (let ((callback (gethash payload (ping-callbacks driver))))
               (when callback
                 (remhash payload (ping-callbacks driver))
                 (funcall callback))))
           :close-callback
           (lambda (data &key start end code)
             (send driver (subseq data start end) :type :close :code code)
             (setf (ready-state driver) :closed)
             (setf (fast-websocket:ws-stage ws) 5))
           :ping-callback
           (lambda (payload &key start end)
             ;; XXX: needless subseq
             (send driver (subseq payload start end) :type :pong))
           :error-callback
           (lambda (code reason)
             (emit :error driver reason)
             (send driver reason :type :close :code code)
             (setf (ready-state driver) :closed)
             (setf (stage driver) 5)
             (emit :close driver (make-close-event :code code :reason reason)))))))

(defmethod version ((driver hybi))
  (format nil "hybi-~A"
          (gethash "sec-websocket-version"
                   (getf (env driver) :headers))))

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

  (let ((frame
          (fast-websocket:compose-frame data :type type :code code :masking (masking driver))))
    (bb:with-promise (resolve reject)
      (write-to-socket (socket driver) frame
                       :callback
                       (lambda () (resolve))))))

@export
(defun generate-accept (key)
  (base64:usb8-array-to-base64-string
   (ironclad:digest-sequence :sha1
                             (ironclad:ascii-string-to-byte-array
                              (concatenate 'string key +guid+)))))

(defmethod handshake-response ((driver hybi))
  (let ((sec-key (gethash "sec-websocket-key"
                          (getf (env driver) :headers))))
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
  (funcall (parser driver) data))
