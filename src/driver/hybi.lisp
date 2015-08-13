(in-package :cl-user)
(defpackage websocket-driver.driver.hybi
  (:use :cl
        #:split-sequence
        #:websocket-driver.driver.base)
  (:import-from :event-emitter
                #:emit)
  (:import-from :fast-websocket
                #:make-ws
                #:ws-stage
                #:make-parser
                #:compose-frame
                #:error-code)
  (:import-from :clack.socket
                #:write-sequence-to-socket
                #:write-sequence-to-socket-buffer
                #:write-byte-to-socket-buffer
                #:flush-socket-buffer)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-sequence
                #:fast-write-byte)
  (:import-from :blackbird
                #:with-promise)
  (:import-from :ironclad
                #:digest-sequence
                #:ascii-string-to-byte-array)
  (:import-from :base64
                #:usb8-array-to-base64-string)
  (:import-from :trivial-utf-8
                #:string-to-utf-8-bytes)
  (:import-from :alexandria
                #:define-constant
                #:when-let)
  (:export #:hybi))
(in-package :websocket-driver.driver.hybi)

(define-constant +guid+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test 'equal)

(defclass hybi (driver)
  ((headers :initarg :headers
            :initform (error ":headers is required")
            :type hash-table
            :accessor headers)
   (require-masking :initarg :require-masking
                    :initform t
                    :accessor require-masking)

   (ws :initform (fast-websocket:make-ws)
       :accessor ws)
   (ping-callbacks :initform (make-hash-table :test 'equalp)
                   :accessor ping-callbacks)
   (parser :accessor parser)))

(defun split-by-comma (string)
  (mapl (lambda (parts)
          (rplaca parts (string-trim '(#\Space) (car parts))))
        (split-sequence #\, string)))

(defmethod initialize-instance :after ((driver hybi) &key)
  (let ((protocols (accept-protocols driver))
        (env-protocols (gethash "sec-websocket-protocol" (headers driver))))
    (when env-protocols
      (setq env-protocols (split-by-comma env-protocols)))
    (setf (protocol driver)
          (find-if (lambda (proto)
                     (find proto protocols :test #'string=))
                   env-protocols)))

  ;; Sec-Websocket-Version must be "13"
  (let ((ws-version (gethash "sec-websocket-version"
                             (headers driver)
                             "")))
    (etypecase ws-version
      (string
       (unless (find "13" (split-by-comma ws-version)
                     :test #'string=)
         (error "Unsupported WebSocket version: ~S" ws-version)))
      (integer
       (unless (= ws-version 13)
         (error "Unsupported WebSocket version: ~S" ws-version)))))
  (setf (version driver) "hybi-13")

  (setf (parser driver)
        (make-parser (ws driver)
                     :require-masking (require-masking driver)
                     :max-length (max-length driver)
                     :message-callback
                     (lambda (message)
                       (emit :message driver message))
                     :pong-callback
                     (lambda (payload)
                       (when-let (callback (gethash payload (ping-callbacks driver)))
                         (remhash payload (ping-callbacks driver))
                         (funcall callback)))
                     :close-callback
                     (lambda (data &key start end code)
                       (send driver (subseq data start end) :type :close :code code)
                       (setf (ready-state driver) :closed)
                       (setf (ws-stage (ws driver)) 0)
                       (emit :close driver :code code :reason (subseq data start end)))
                     :ping-callback
                     (lambda (payload &key start end)
                       ;; XXX: needless subseq
                       (send driver (subseq payload start end) :type :pong))
                     :error-callback
                     (lambda (code reason)
                       (emit :error driver reason)
                       (send driver reason :type :close :code code)
                       (setf (ready-state driver) :closed)
                       (emit :close driver :code code :reason reason)))))

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
     (emit :close driver :code code :reason reason)
     t)
    (:open
     (send driver reason :type :close :code code)
     (setf (ready-state driver) :closing)
     t)
    (otherwise nil)))

(defmethod send ((driver hybi) data &key type code)
  (let ((frame (compose-frame data
                              :type type
                              :code code
                              :masking nil)))
    (bb:with-promise (resolve reject)
      (write-sequence-to-socket (socket driver) frame
                                :callback
                                (lambda () (resolve))))))

(defun generate-accept (key)
  (declare (optimize (speed 3) (safety 0))
           (type simple-string key))
  (base64:usb8-array-to-base64-string
   (ironclad:digest-sequence :sha1
                             (ironclad:ascii-string-to-byte-array
                              (concatenate 'string key +guid+)))))

(defmethod send-handshake-response ((driver hybi) &key callback)
  (let ((socket (socket driver))
        (sec-key (gethash "sec-websocket-key" (headers driver))))
    (unless (stringp sec-key)
      (when callback (funcall callback))
      (return-from send-handshake-response))

    (labels ((octets (data)
               (write-sequence-to-socket-buffer socket data))
             (ascii-string (data)
               (octets (ascii-string-to-byte-array data)))
             (crlf ()
               (octets #.(ascii-string-to-byte-array (format nil "~C~C" #\Return #\Newline)))))
      (octets
       #.(ascii-string-to-byte-array
          (with-output-to-string (s)
            (format s "HTTP/1.1 101 Switching Protocols~C~C" #\Return #\Newline)
            (format s "Upgrade: websocket~C~C" #\Return #\Newline)
            (format s "Connection: Upgrade~C~C" #\Return #\Newline)
            (format s "Sec-WebSocket-Accept: "))))
      (ascii-string
       (generate-accept sec-key))
      (crlf)

      (when-let (protocol (protocol driver))
        (octets
         #.(ascii-string-to-byte-array "Sec-WebSocket-Protocol: "))
        (ascii-string protocol)
        (crlf))

      (loop for (name . value) in (additional-headers driver)
            do (ascii-string
                (string-capitalize name))
               (octets
                #.(ascii-string-to-byte-array ": "))
               (ascii-string value)
               (crlf))

      (crlf))

    (flush-socket-buffer socket :callback callback)))

(defmethod parse ((driver hybi) data &key (start 0) end)
  (funcall (parser driver) data :start start :end end))
