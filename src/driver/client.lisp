(in-package :cl-user)
(defpackage websocket-driver.driver.client
  (:use :cl
        :event-emitter
        :websocket-driver.driver.base
        :websocket-driver.events
        :websocket-driver.error)
  (:import-from :websocket-driver.driver.hybi
                :hybi
                :generate-accept
                :protocols
                :buffer)
  (:import-from :websocket-driver.header
                :finalize-headers)
  (:import-from :websocket-driver.socket
                :write-to-socket)
  (:import-from :fast-io
                :with-fast-output
                :with-fast-input
                :fast-write-sequence
                :fast-write-byte
                :fast-read-byte
                :make-output-buffer
                :finish-output-buffer
                :output-buffer-len)
  (:import-from :cl-async-future
                :make-future
                :finish)
  (:import-from :iolib
                :make-socket
                :connect
                :lookup-hostname
                :event-dispatch)
  (:import-from :bordeaux-threads
                :make-thread
                :*default-special-bindings*)
  (:import-from :babel
                :string-to-octets
                :octets-to-string)
  (:import-from :base64
                :usb8-array-to-base64-string))
(in-package :websocket-driver.driver.client)

(syntax:use-syntax :annot)

(defconstant +cr+ #x0d)
(defconstant +lf+ #x0A)

(defconstant +max-line-length+ 4096)

(defparameter *status-line*
  (ppcre:create-scanner "^(HTTP\\/[0-9]+\\.[0-9]+) ([0-9]{3}) ([\\x20-\\x7e]+)$"
                        :multi-line-mode t))

(defparameter *header-line*
  (ppcre:create-scanner
   "^([!#\\$%&'\\*\\+\\-\\.\\^_`\\|~0-9a-z]+):\\s*([\\x20-\\x7e]*?)\\s*$"
   :multi-line-mode t
   :case-insensitive-mode t))

(defun generate-key ()
  (let ((key (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16)
      (setf (aref key i) (random 255)))
    (base64:usb8-array-to-base64-string key)))

@export
(defclass client (hybi)
  ((url :initarg :url
        :accessor url)
   (key :initform (generate-key)
        :accessor key)
   (accept :accessor accept)
   (buffer :initform (fast-io::make-output-buffer)
           :accessor buffer)
   (parsed-headers :initform (make-hash-table :test 'equal)
                   :accessor parsed-headers)
   (http-stage :type fixnum
               :initform 0
               :accessor http-stage)
   (ready-state :type fixnum
                :initform -1)))

(defmethod initialize-instance :after ((driver client) &key)
  (setf (url driver) (quri:uri (url driver)))

  (setf (accept driver) (generate-accept (key driver))))

(defmethod version ((driver client))
  "hybi-13")

(defmethod start-connection ((driver client))
  (when (ready-state driver)
    (return-from start-connection nil))

  (setf (ready-state driver) :connecting)

  (let ((socket (iolib:make-socket :connect :active
                                   :address-family :internet
                                   :type :stream
                                   :external-format '(:utf-8 :eol-style :crlf)
                                   :ipv6 nil))
        (uri (url driver)))
    (iolib:connect socket
                   (iolib:lookup-hostname (quri:uri-host uri))
                   :port (quri:uri-port uri)
                   :wait t)
    (setf (socket driver) socket))

  (set-read-callback driver
                     (lambda (data)
                       (parse driver data)))

  (let ((bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                         (*error-output* . ,*error-output*))))
    (bt:make-thread
     (lambda ()
       (iolib:event-dispatch (event-base driver)))))

  (let ((future (asf:make-future)))
    (write-to-socket (socket driver)
                     (handshake-request driver)
                     :callback
                     (lambda () (asf:finish future)))
    future))

(defmethod parse ((driver client) data)
  (let ((ready-state (ready-state driver)))
    (unless (or (null ready-state)
                (eq ready-state :connecting))
      (return-from parse (call-next-method))))

  (let (code)
    (handler-case
        (labels ((fail-handshake (format-control &rest format-arguments)
                   (error 'protocol-error
                          :format-control (concatenate 'string "Error during WebSocket handshake: " format-control)
                          :format-arguments format-arguments))
                 (parse-start-line (data)
                   (multiple-value-bind (match regs)
                       (ppcre:scan-to-strings *status-line* (octets-to-string data :encoding :utf-8))
                     (when match
                       (setq code
                             (or (parse-integer (aref regs 1) :junk-allowed t)
                                 (aref regs 1)))
                       T)))
                 (parse-header-line (data)
                   (multiple-value-bind (match regs)
                       (ppcre:scan-to-strings *header-line* (octets-to-string data :encoding :utf-8))
                     (when match
                       (setf (gethash (aref regs 0) (parsed-headers driver))
                             (aref regs 1))
                       (set-header driver
                                   (aref regs 0)
                                   (aref regs 1))
                       T)))
                 (validate-handshake ()
                   (unless (eql code 101)
                     (fail-handshake "Unexpected response code: ~S"
                                     code))

                   (let ((upgrade (gethash "Upgrade" (parsed-headers driver) "")))
                     (cond
                       ((string= upgrade "")
                        (fail-handshake "'Upgrade' header is missing"))
                       ((not (string-equal upgrade "websocket"))
                        (fail-handshake "'Upgrade' header value is not 'WebSocket'"))))

                   (let ((connection (gethash "Connection" (parsed-headers driver) "")))
                     (cond
                       ((string= connection "")
                        (fail-handshake "'Connection' header is missing"))
                       ((not (string-equal connection "upgrade"))
                        (fail-handshake "'Connection' header value is not 'Upgrade'"))))

                   (unless (string= (accept driver)
                                    (gethash "Sec-WebSocket-Accept" (parsed-headers driver) ""))
                     (fail-handshake "Sec-WebSocket-Accept mismatch"))

                   (let ((protocol (gethash "Sec-WebSocket-Protocol" (parsed-headers driver) "")))
                     (unless (string= protocol "")
                       (if (find protocol (protocols driver) :test #'string=)
                           (setf (protocol driver) protocol)
                           (fail-handshake "Sec-WebSocket-Protocol mismatch"))))))
          (progn
            (with-fast-input (buffer data)
              (do ((byte (fast-read-byte buffer nil nil)
                         next)
                   (next (fast-read-byte buffer nil nil)
                         (fast-read-byte buffer nil nil)))
                  ((null byte))
                (cond
                  ((and (= byte +cr+)
                        (= next +lf+)
                        (< (http-stage driver) 2))
                   (if (zerop (fast-io::output-buffer-len (buffer driver)))
                       (when (= (http-stage driver) 1)
                         (setf (http-stage driver) 2))
                       (if (case (http-stage driver)
                             (0 (parse-start-line (fast-io::finish-output-buffer (buffer driver))))
                             (1 (parse-header-line (fast-io::finish-output-buffer (buffer driver)))))
                           (setf (http-stage driver) 1)
                           (fail-handshake "Invalid HTTP response")))
                   (setq next (fast-read-byte buffer nil nil))
                   (setf (buffer driver) (fast-io::make-output-buffer)))
                  (T
                   (when (<= 0 (http-stage driver))
                     (fast-write-byte byte (buffer driver))
                     (when (and (< (http-stage driver) 2)
                                (< +max-line-length+ (fast-io::output-buffer-len (buffer driver))))
                       (fail-handshake "Invalid HTTP response")))))))

            (when (= (http-stage driver) 2)
              (validate-handshake)
              (open-connection driver))

            (when (eq (ready-state driver) :open)
              (parse driver (fast-io::finish-output-buffer (buffer driver))))))

      (protocol-error (e)
        (setf (http-stage driver) -1)
        (emit :error driver e)
        (setf (ready-state driver) :closed)
        (emit :close driver
              (make-close-event :code (error-code e) :reason (princ-to-string e)))))))

(defmethod handshake-request ((driver client))
  (let ((uri (url driver)))
    (with-fast-output (buffer :vector)
      (fast-write-sequence
       (string-to-octets (format nil "GET ~:[/~;~:*~A~]~:[~;~:*?~A~] HTTP/1.1~C~C"
                                 (quri:uri-path uri)
                                 (quri:uri-query uri)
                                 #\Return #\Linefeed)
                         :encoding :utf-8)
       buffer)
      (fast-write-sequence
       (string-to-octets (format nil "Host: ~A~:[~;~:*:~D~]~C~C"
                                 (quri:uri-host uri)
                                 (quri:uri-port uri)
                                 #\Return #\Linefeed)
                         :encoding :utf-8)
       buffer)
      (fast-write-sequence
       #.(string-to-octets
          (with-output-to-string (s)
            (format s "Upgrade: websocket~C~C" #\Return #\Linefeed)
            (format s "Connection: Upgrade~C~C" #\Return #\Linefeed)))
       buffer)
      (fast-write-sequence
       (string-to-octets
        (format nil "Sec-WebSocket-Key: ~A~C~C" (key driver) #\Return #\Linefeed))
       buffer)
      (fast-write-sequence
       #.(string-to-octets
          (format nil "Sec-WebSocket-Version: 13~C~C" #\Return #\Linefeed))
       buffer)
      (when (protocols driver)
        (fast-write-sequence
         (string-to-octets
          (format nil "Sec-WebSocket-Protocol: ~{~A~^, ~}~C~C"
                  (protocols driver)
                  #\Return #\Linefeed)
          :encoding :utf-8)
         buffer))

      (fast-write-sequence (finalize-headers (headers driver))
                           buffer)

      (fast-write-sequence #.(string-to-octets (format nil "~C~C" #\Return #\Linefeed))
                           buffer))))
