(in-package :cl-user)
(defpackage websocket-driver.driver.base
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
  (:export #:driver
           #:socket
           #:additional-headers
           #:accept-protocols
           #:protocol
           #:version
           #:max-length
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
           #:send-handshake-request))
(in-package :websocket-driver.driver.base)

(defparameter +states+
  #(:connecting :open :closing :closed))

(defclass driver (event-emitter)
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
   (ws :initform (fast-websocket:make-ws)
       :accessor ws)
   (ping-callbacks :initform (make-hash-table :test 'equalp)
                   :accessor ping-callbacks)
   (parser :accessor parser)))

(defun send-close-frame (driver reason code)
  (setf (ready-state driver) :closing)
  (send driver reason :type :close :code code
                      :callback
                      (lambda ()
                        (close-connection driver reason code))))

(defmethod initialize-instance :after ((driver driver) &key)
  (setf (parser driver)
        (make-parser (ws driver)
                     :require-masking (require-masking driver)
                     :max-length (max-length driver)
                     :message-callback
                     (lambda (message)
                       (emit :message driver message))
                     :ping-callback
                     (lambda (payload)
                       (send driver payload :type :pong))
                     :pong-callback
                     (lambda (payload)
                       (let ((callback (gethash payload (ping-callbacks driver))))
                         (when callback
                           (remhash payload (ping-callbacks driver)) 
                           (funcall callback))))
                     :close-callback
                     (lambda (data &key code)
                       (case (ready-state driver)
                         ;; closing request by the other peer
                         (:open
                          (send-close-frame driver data code))
                         ;; probably the response for a 'close' frame
                         (otherwise
                          (close-connection driver data code)))
                       (setf (ws-stage (ws driver)) 0))
                     :error-callback
                     (lambda (code reason)
                       (emit :error driver reason)
                       (send-close-frame driver reason code)
                       (setf (ws-stage (ws driver)) 0)))))

(defgeneric ready-state (driver)
  (:method ((driver driver))
    (aref +states+ (slot-value driver 'ready-state))))

(defgeneric (setf ready-state) (state driver)
  (:method (state (driver driver))
    (setf (slot-value driver 'ready-state)
          (ecase state
            (:connecting 0)
            (:open       1)
            (:closing    2)
            (:closed     3)))))

(defgeneric start-connection (driver))

(defgeneric parse (driver data &key start end)
  (:method (driver data &key start end)
    (funcall (parser driver) data :start start :end end)))

(defgeneric send (driver data &key start end type code callback))
(defmethod send :around ((driver driver) data &rest args)
  (when (eq (ready-state driver) :connecting)
    (return-from send
      (enqueue driver (cons data args))))

  (unless (eq (ready-state driver) :open)
    (return-from send nil))

  (call-next-method))

(defgeneric send-text (driver message &key start end callback)
  (:method ((driver driver) message &rest args)
    (apply #'send driver message :type :text args)))

(defgeneric send-binary (driver message &key start end callback)
  (:method ((driver driver) message &rest args)
    (apply #'send driver message :type :binary args)))

(defgeneric send-ping (driver &optional message callback)
  (:method ((driver driver) &optional message callback)
    (unless message
      (setq message #.(make-array 0 :element-type '(unsigned-byte 8))))
    (when callback
      (setf (gethash message (ping-callbacks driver))
            callback))
    (send driver message :type :ping)))

(defgeneric close-connection (driver &optional reason code))

(defmethod close-connection :around ((driver driver) &optional reason code)
  (case (ready-state driver)
    (:connecting
     (setf (ready-state driver) :closed)
     (emit :close driver :code code :reason reason)
     t)
    (:open
     (call-next-method))
    (otherwise nil)))

(defgeneric open-connection (driver)
  (:method ((driver driver))
    (setf (ready-state driver) :open)

    (map nil (lambda (message)
               (apply #'send driver message))
         (queue driver))

    (setf (queue driver)
          (make-array 0 :adjustable t :fill-pointer 0))

    (emit :open driver)))

(defun enqueue (driver message)
  (vector-push-extend message (queue driver))
  t)

(defgeneric send-handshake-response (driver &key callback))

(defgeneric send-handshake-request (driver &key callback))
