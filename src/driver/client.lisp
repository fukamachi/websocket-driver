(in-package :cl-user)
(defpackage websocket-driver.driver.client
  (:use :cl
        #:websocket-driver.driver.base
        #:websocket-driver.util)
  (:import-from :cl-async
                #:tcp-connect)
  (:import-from :event-emitter
                #:emit)
  (:import-from :fast-io
                #:with-fast-output
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
(in-package :websocket-driver.driver.client)

(defclass client (driver)
  ((url :initarg :url
        :initform (error ":url is required")
        :accessor url)
   (key :initform (generate-key)
        :reader key)
   (accept :accessor accept)
   (version :initform "hybi-13")
   (require-masking :initarg :require-masking
                    :initform nil
                    :accessor require-masking)))

(defun generate-key ()
  (let ((key (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref key i) (random 255)))
    (base64:usb8-array-to-base64-string key)))

(defmethod initialize-instance :after ((driver client) &key)
  (setf (accept driver) (generate-accept (key driver))))

(defmethod start-connection ((driver client))
  (unless (eq (ready-state driver) :connecting)
    (return-from start-connection))

  (flet ((fail-handshake (format-control &rest format-arguments)
           (error 'protocol-error
                  :format-control (format nil "Error during WebSocket handshake:~%  ~A" format-control)
                  :format-arguments format-arguments)))
    (let* ((uri (quri:uri (url driver)))
           (connect-fn (cond
                         ((string-equal (uri-scheme uri) "ws")
                          #'as:tcp-connect)
                         ((string-equal (uri-scheme uri) "wss")
                          #'as-ssl:tcp-ssl-connect)
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

                                       (unless (string= (accept driver)
                                                        (gethash "sec-websocket-accept" headers ""))
                                         (fail-handshake "Sec-WebSocket-Accept mismatch"))

                                       (let ((protocol (gethash "sec-websocket-protocol" headers)))
                                         (when (accept-protocols driver)
                                           (unless (and protocol
                                                        (find protocol (accept-protocols driver) :test #'string=))
                                             (fail-handshake "Sec-WebSocket-Protocol mismatch"))
                                           (setf (protocol driver) protocol))))))
           (socket
             (funcall connect-fn (uri-host uri) (uri-port uri)
                      (lambda (sock data)
                        (funcall http-parser data)
                        (let ((callbacks (as::get-callbacks (as::socket-c sock))))
                          (setf (getf callbacks :read-cb)
                                (lambda (sock data)
                                  (declare (ignore sock))
                                  (parse driver data)))
                          (as::save-callbacks (as::socket-c sock) callbacks))
                        (open-connection driver)))))

      (setf (socket driver) socket)
      (send-handshake-request driver))))

(defmethod send ((driver client) data &key start end type code callback)
  (let ((frame (compose-frame data
                              :start start
                              :end end
                              :type type
                              :code code
                              :masking t)))
    (as:write-socket-data (socket driver) frame
                          :write-cb callback)))

(defmethod send-handshake-request ((driver client) &key callback)
  (let ((uri (quri:uri (url driver)))
        (socket (socket driver)))
    (as:write-socket-data
     socket
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
                  (key driver)
                  #\Return #\Newline))
         (octets
          #.(ascii-string-to-byte-array
             (format nil "Sec-WebSocket-Version: 13~C~C" #\Return #\Newline)))
         (when (accept-protocols driver)
           (ascii-string
            (format nil "Sec-WebSocket-Protocol: ~{~A~^, ~}~C~C"
                    (accept-protocols driver)
                    #\Return #\Newline)))

         (loop for (name . value) in (additional-headers driver)
               do (ascii-string
                   (string-capitalize name))
                  (octets
                   #.(ascii-string-to-byte-array ": "))
                  (ascii-string value)
                  (crlf))

         (crlf)))
     :write-cb callback)))

(defmethod close-connection ((driver client) &optional reason code)
  (as:close-socket (socket driver))
  (setf (ready-state driver) :closed)
  (emit :close driver :code code :reason reason)
  t)
