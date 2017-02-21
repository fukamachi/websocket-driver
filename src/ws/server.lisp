(in-package :cl-user)
(defpackage websocket-driver.ws.server
  (:use :cl
        #:websocket-driver.ws.base
        #:websocket-driver.util)
  (:import-from :event-emitter
                #:emit)
  (:import-from :fast-websocket
                #:compose-frame
                #:error-code)
  (:import-from :clack.socket
                #:read-callback
                #:write-sequence-to-socket
                #:write-sequence-to-socket-buffer
                #:write-byte-to-socket-buffer
                #:flush-socket-buffer
                #:close-socket
                #:socket-async-p
                #:socket-stream)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-sequence
                #:fast-write-byte)
  (:import-from :ironclad
                #:ascii-string-to-byte-array)
  (:import-from :trivial-utf-8
                #:string-to-utf-8-bytes)
  (:export #:server))
(in-package :websocket-driver.ws.server)

(defclass server (ws)
  ((headers :initarg :headers
            :initform (error ":headers is required")
            :type hash-table
            :accessor headers)
   (require-masking :initarg :require-masking
                    :initform t
                    :accessor require-masking)))

(defmethod initialize-instance :after ((server server) &key)
  (let ((protocols (accept-protocols server))
        (env-protocols (gethash "sec-websocket-protocol" (headers server))))
    (when env-protocols
      (setq env-protocols (split-by-comma env-protocols)))
    (setf (protocol server)
          (find-if (lambda (proto)
                     (find proto protocols :test #'string=))
                   env-protocols)))

  ;; Sec-Websocket-Version must be "13"
  (let ((ws-version (gethash "sec-websocket-version"
                             (headers server)
                             "")))
    (etypecase ws-version
      (string
       (unless (find "13" (split-by-comma ws-version)
                     :test #'string=)
         (error "Unsupported WebSocket version: ~S" ws-version)))
      (integer
       (unless (= ws-version 13)
         (error "Unsupported WebSocket version: ~S" ws-version)))))
  (setf (version server) "hybi-13"))

(defmethod start-connection ((server server))
  (unless (eq (ready-state server) :connecting)
      (return-from start-connection))

  (let ((socket (socket server)))
    (setf (read-callback socket)
          (lambda (data &key (start 0) end)
            (parse server data :start start :end end)))

    (send-handshake-response server
                             :callback
                             (lambda ()
                               (unless (eq (ready-state server) :closed)
                                 (open-connection server))))

    (unless (clack.socket:socket-async-p socket)
      (unwind-protect
           (loop with stream = (socket-stream socket)
                 for frame = (read-websocket-frame stream)
                 while frame
                 do (funcall (read-callback socket) frame))
        (clack.socket:close-socket socket)))))

(defmethod close-connection ((server server) &optional (reason "") (code (error-code :normal-closure)))
  (close-socket (socket server))
  (send server reason :type :close :code code)
  (setf (ready-state server) :closing)
  t)

(defmethod send ((server server) data &key start end type code callback)
  (let ((frame (compose-frame data
                              :start start
                              :end end
                              :type type
                              :code code
                              :masking nil)))
    (write-sequence-to-socket (socket server) frame
                              :callback callback)))

(defmethod send-handshake-response ((server server) &key callback)
  (let ((socket (socket server))
        (sec-key (gethash "sec-websocket-key" (headers server))))
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

      (let ((protocol (protocol server)))
        (when protocol
          (octets
           #.(ascii-string-to-byte-array "Sec-WebSocket-Protocol: "))
          (ascii-string protocol)
          (crlf)))

      (loop for (name . value) in (additional-headers server)
            do (ascii-string
                (string-capitalize name))
               (octets
                #.(ascii-string-to-byte-array ": "))
               (ascii-string value)
               (crlf))

      (crlf))

    (flush-socket-buffer socket :callback callback)))
