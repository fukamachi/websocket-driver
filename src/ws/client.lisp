(in-package :cl-user)
(defpackage websocket-driver.ws.client
  (:use :cl
        #:websocket-driver.ws.base
        #:websocket-driver.util)
  (:import-from :event-emitter
                #:emit)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-byte
                #:fast-write-sequence)
  (:import-from :fast-websocket
                #:compose-frame)
  (:import-from :fast-http
                #:make-http-response
                #:make-parser
                #:http-status)
  (:import-from :cl-base64
                #:usb8-array-to-base64-string)
  (:import-from :trivial-utf-8
                #:string-to-utf-8-bytes)
  (:import-from :ironclad
                #:ascii-string-to-byte-array)
  (:import-from :quri
                #:uri
                #:uri-scheme
                #:uri-host
                #:uri-port)
  (:export #:client))
(in-package :websocket-driver.ws.client)

(defclass client (ws)
  ((url :initarg :url
        :initform (error ":url is required")
        :accessor url)
   (key :initform (generate-key)
        :reader key)
   (accept :accessor accept)
   (version :initform "hybi-13")
   (require-masking :initarg :require-masking
                    :initform nil
                    :accessor require-masking)
   (read-thread :initform nil)))

(defun generate-key ()
  (let ((key (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref key i) (random 255)))
    (base64:usb8-array-to-base64-string key)))

(defmethod initialize-instance :after ((client client) &key)
  (setf (accept client) (generate-accept (key client))))

(defun read-until-crlf*2 (stream)
  (declare (optimize (speed 3)))
  (with-fast-output (buf)
    (tagbody
     read-cr
       (loop for byte of-type (or (unsigned-byte 8) null) = (read-byte stream nil nil)
             if byte
               do (fast-write-byte byte buf)
             else
               do (go eof)
             until (= byte (char-code #\Return)))

     read-lf
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf)
              (go read-cr2))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-cr2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf2))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-lf2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     eof)))

(defmethod start-connection ((client client))
  (unless (eq (ready-state client) :connecting)
    (return-from start-connection))

  (flet ((fail-handshake (format-control &rest format-arguments)
           (error (format nil "Error during WebSocket handshake:~%  ~A"
                          (apply #'format nil format-control format-arguments)))))
    (let* ((uri (quri:uri (url client)))
           (secure (cond ((string-equal (uri-scheme uri) "ws")
                          nil)
                         ((string-equal (uri-scheme uri) "wss")
                          t)
                         (t (error "Invalid URI scheme: ~S" (uri-scheme uri)))))
           (http (make-http-response))
           (http-parser (make-parser http
                                     :first-line-callback
                                     (lambda ()
                                       (unless (= (fast-http:http-status http) 101)
                                         (fail-handshake "Unexpected response code: ~S"
                                                         (fast-http:http-status http))))
                                     :header-callback
                                     (lambda (headers)
                                       (let ((upgrade (gethash "upgrade" headers)))
                                         (cond
                                           ((null upgrade)
                                            (fail-handshake "'Upgrade' header is missing"))
                                           ((not (string-equal upgrade "websocket"))
                                            (fail-handshake "'Upgrade' header value is not 'WebSocket'"))))
                                       (let ((connection (gethash "connection" headers)))
                                         (cond
                                           ((null connection)
                                            (fail-handshake "'Connection' header is missing"))
                                           ((not (string-equal connection "upgrade"))
                                            (fail-handshake "'Connection' header value is not 'Upgrade'"))))

                                       (unless (string= (accept client)
                                                        (gethash "sec-websocket-accept" headers ""))
                                         (fail-handshake "Sec-WebSocket-Accept mismatch"))

                                       (let ((protocol (gethash "sec-websocket-protocol" headers)))
                                         (when (accept-protocols client)
                                           (unless (and protocol
                                                        (find protocol (accept-protocols client) :test #'string=))
                                             (fail-handshake "Sec-WebSocket-Protocol mismatch"))
                                           (setf (protocol client) protocol))))))
           (stream (usocket:socket-stream
                    (usocket:socket-connect (uri-host uri) (uri-port uri)
                                            :element-type '(unsigned-byte 8))))
           (stream (if secure
                       #+websocket-driver-no-ssl
                       (error "SSL not supported. Remove :websocket-driver-no-ssl from *features* to enable SSL.")
                       #-websocket-driver-no-ssl
                       (progn
                         (cl+ssl:ensure-initialized)
                         (setf (cl+ssl:ssl-check-verify-p) t)
                         (let ((ctx (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-peer+
                                                         :verify-location :default)))
                           ;; TODO: certificate files
                           (cl+ssl:with-global-context (ctx :auto-free-p t)
                             (cl+ssl:make-ssl-client-stream stream
                                                            :hostname (uri-host uri)))))
                       stream)))

      (setf (socket client) stream)
      (send-handshake-request client)
      (funcall http-parser (read-until-crlf*2 stream))
      (open-connection client)
      (setf (slot-value client 'read-thread)
            (bt:make-thread
             (lambda ()
               (unwind-protect
                    (loop for frame = (read-websocket-frame stream)
                          while frame
                          do (parse client frame))
                 (close-connection client)))
             :name "websocket client read thread"
             :initial-bindings `((*standard-output* . ,*standard-output*)
                                 (*error-output* . ,*error-output*))))
      client)))

(defmethod send ((client client) data &key start end type code callback)
  (let ((frame (compose-frame data
                              :start start
                              :end end
                              :type type
                              :code code
                              :masking t)))
    (handler-case (progn
                    (write-sequence frame (socket client))
                    (force-output (socket client)))
      (error ()
        (close-connection client)))
    (when callback
      (funcall callback))))

(defmethod send-handshake-request ((client client) &key callback)
  (let ((uri (quri:uri (url client)))
        (socket (socket client)))
    (write-sequence
     (with-fast-output (buffer)
       (labels ((octets (data)
                  (fast-write-sequence data buffer))
                (ascii-string (data)
                  (octets (ascii-string-to-byte-array data)))
                (crlf ()
                  (octets #.(ascii-string-to-byte-array (format nil "~C~C" #\Return #\Newline)))))
         (ascii-string
          (format nil "GET ~:[/~;~:*~A~]~:[~;~:*?~A~] HTTP/1.1~C~C"
                  (quri:uri-path uri)
                  (quri:uri-query uri)
                  #\Return #\Newline))
         (ascii-string
          (format nil "Host: ~A~C~C"
                  (quri:uri-authority uri)
                  #\Return #\Newline))
         (octets
          #.(ascii-string-to-byte-array
             (with-output-to-string (s)
               (format s "Upgrade: websocket~C~C" #\Return #\Newline)
               (format s "Connection: Upgrade~C~C" #\Return #\Newline))))
         (ascii-string
          (format nil "Sec-WebSocket-Key: ~A~C~C"
                  (key client)
                  #\Return #\Newline))
         (octets
          #.(ascii-string-to-byte-array
             (format nil "Sec-WebSocket-Version: 13~C~C" #\Return #\Newline)))
         (when (accept-protocols client)
           (ascii-string
            (format nil "Sec-WebSocket-Protocol: ~{~A~^, ~}~C~C"
                    (accept-protocols client)
                    #\Return #\Newline)))

         (loop for (name . value) in (additional-headers client)
               do (ascii-string
                   (string-capitalize name))
                  (octets
                   #.(ascii-string-to-byte-array ": "))
                  (ascii-string value)
                  (crlf))

         (crlf)))
     socket)
    (force-output socket)
    (when callback
      (funcall callback))))

(defmethod close-connection ((client client) &optional reason code)
  (ignore-errors (close (socket client)))
  (setf (ready-state client) :closed)
  (let ((thread (slot-value client 'read-thread)))
    (when thread
      (unless (eq (bt:current-thread) thread)
        (bt:destroy-thread thread))
      (setf (slot-value client 'read-thread) nil)))
  (emit :close client :code code :reason reason)
  t)
