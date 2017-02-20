(in-package :cl-user)
(defpackage websocket-driver.ws.base
  (:use :cl)
  (:import-from :fast-websocket
                #:make-ws
                #:ws-stage
                #:make-parser)
  (:import-from :event-emitter
                #:emit
                #:event-emitter)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-sequence)
  (:import-from :bordeaux-threads
                #:make-recursive-lock
                #:with-recursive-lock-held)
  (:export #:ws
           #:socket
           #:additional-headers
           #:accept-protocols
           #:protocol
           #:version
           #:ready-state
           #:require-masking
           #:ws

           #:start-connection
           #:parse
           #:send
           #:send-text
           #:send-binary
           #:send-ping
           #:close-connection
           #:open-connection
           #:send-handshake-response
           #:send-handshake-request

           #:read-websocket-message))
(in-package :websocket-driver.ws.base)

(defparameter +states+
  #(:connecting :open :closing :closed))

(defclass ws (event-emitter)
  ((socket :initarg :socket
           :accessor socket)
   (accept-protocols :initarg :accept-protocols
                     :initform '()
                     :accessor accept-protocols)
   (protocol :type (or null string)
             :initform nil
             :accessor protocol)
   (version :accessor version)
   (max-length :initarg :max-length
               :initform #x3ffffff
               :accessor max-length)
   (ready-state :type fixnum
                :initform 0)
   (additional-headers :initarg :additional-headers
                       :initform '()
                       :accessor additional-headers)

   (queue :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor queue)

   (require-masking :initarg :require-masking
                    :accessor require-masking)
   (ws-parse :initform (fast-websocket:make-ws)
             :accessor ws-parse)
   (ping-callbacks :initform (make-hash-table :test 'equalp)
                   :accessor ping-callbacks)
   (parser :accessor parser)
   (parse-lock :initform (make-recursive-lock)
               :reader parse-lock)))

(defun send-close-frame (ws reason code)
  (setf (ready-state ws) :closing)
  (send ws reason :type :close :code code
                  :callback
                  (lambda ()
                    (close-connection ws reason code))))

(defmethod initialize-instance :after ((ws ws) &key)
  (setf (parser ws)
        (make-parser (ws-parse ws)
                     :require-masking (require-masking ws)
                     :max-length (max-length ws)
                     :message-callback
                     (lambda (message)
                       (emit :message ws message))
                     :ping-callback
                     (lambda (payload)
                       (send ws payload :type :pong))
                     :pong-callback
                     (lambda (payload)
                       (let ((callback (gethash payload (ping-callbacks ws))))
                         (when callback
                           (remhash payload (ping-callbacks ws)) 
                           (funcall callback))))
                     :close-callback
                     (lambda (data &key code)
                       (case (ready-state ws)
                         ;; closing request by the other peer
                         (:open
                          (send-close-frame ws data code))
                         ;; probably the response for a 'close' frame
                         (otherwise
                          (close-connection ws data code)))
                       (setf (ws-stage (ws-parse ws)) 0))
                     :error-callback
                     (lambda (code reason)
                       (emit :error ws reason)
                       (send-close-frame ws reason code)
                       (setf (ws-stage (ws-parse ws)) 0)))))

(defgeneric ready-state (ws)
  (:method ((ws ws))
    (aref +states+ (slot-value ws 'ready-state))))

(defgeneric (setf ready-state) (state ws)
  (:method (state (ws ws))
    (setf (slot-value ws 'ready-state)
          (ecase state
            (:connecting 0)
            (:open       1)
            (:closing    2)
            (:closed     3)))))

(defgeneric start-connection (ws))

(defgeneric parse (ws data &key start end)
  (:method (ws data &key start end)
    (with-recursive-lock-held ((parse-lock ws))
      (funcall (parser ws) data :start start :end end))))

(defgeneric send (ws data &key start end type code callback))
(defmethod send :around ((ws ws) data &rest args)
  (when (eq (ready-state ws) :connecting)
    (return-from send
      (enqueue ws (cons data args))))

  (unless (eq (ready-state ws) :open)
    (return-from send nil))

  (call-next-method))

(defun send-text (ws message &rest args &key start end callback)
  (declare (ignore start end callback))
  (apply #'send ws message :type :text args))

(defun send-binary (ws message &rest args &key start end callback)
  (declare (ignore start end callback))
  (apply #'send ws message :type :binary args))

(defgeneric send-ping (ws &optional message callback)
  (:method ((ws ws) &optional message callback)
    (unless message
      (setq message #.(make-array 0 :element-type '(unsigned-byte 8))))
    (when callback
      (setf (gethash message (ping-callbacks ws))
            callback))
    (send ws message :type :ping)))

(defgeneric close-connection (ws &optional reason code))

(defmethod close-connection :around ((ws ws) &optional reason code)
  (case (ready-state ws)
    (:connecting
     (setf (ready-state ws) :closed)
     (emit :close ws :code code :reason reason)
     t)
    (:open
     (call-next-method))
    (otherwise nil)))

(defgeneric open-connection (ws)
  (:method ((ws ws))
    (setf (ready-state ws) :open)

    (unless (= 0 (length (queue ws)))
      (map nil (lambda (message)
                 (apply #'send ws message))
           (queue ws))

      (setf (queue ws)
            (make-array 0 :adjustable t :fill-pointer 0)))

    (emit :open ws)))

(defun enqueue (ws message)
  (vector-push-extend message (queue ws))
  t)

(defgeneric send-handshake-response (ws &key callback))

(defgeneric send-handshake-request (ws &key callback))

(defun read-websocket-message (stream)
  (let ((buf (make-array 2 :element-type '(unsigned-byte 8)))
        (extended-buf (make-array 8 :element-type '(unsigned-byte 8))))
    (block nil
      (tagbody retry
         (let ((read-bytes (handler-case (read-sequence buf stream)
                             (error ()
                               ;; Retry when I/O timeout error
                               (go retry)))))
           (when (= read-bytes 0)
             (return nil))

           (let ((maskp (plusp (ldb (byte 1 7) (aref buf 1))))
                 (data-length (ldb (byte 7 0) (aref buf 1))))
             (cond
               ((<= 0 data-length 125))
               (t
                (let ((end (if (= data-length 126) 2 8)))
                  (read-sequence extended-buf stream :end end)
                  (incf read-bytes end)
                  (setf data-length
                        (loop with length = 0
                              for i from 0 to end
                              do (incf length (+ (ash length 8) (aref extended-buf i)))
                              finally (return length))))))
             (when maskp
               (incf data-length 4))
             (let ((data (make-array (+ read-bytes data-length) :element-type '(unsigned-byte 8))))
               (replace data buf :end2 2)
               (unless (= read-bytes 2)
                 (replace data extended-buf :start1 2 :end2 (- read-bytes 2)))
               (handler-case
                   (read-sequence data stream :start read-bytes)
                 (error ()
                   (return nil)))
               (return data))))))))
