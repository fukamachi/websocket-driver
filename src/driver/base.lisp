(in-package :cl-user)
(defpackage websocket-driver.driver.base
  (:use :cl)
  (:import-from :event-emitter
                #:emit
                #:event-emitter)
  (:import-from :clack.socket
                #:set-read-callback)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-sequence)
  (:import-from :alexandria
                #:define-constant)
  (:export #:driver
           #:socket
           #:additional-headers
           #:accept-protocols
           #:protocol
           #:version
           #:max-length
           #:ready-state

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

(define-constant +states+
  #(:connecting :open :closing :closed)
  :test 'equalp)

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
          :accessor queue)))

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

(defgeneric start-connection (driver)
  (:method ((driver driver))
    (unless (eq (ready-state driver) :connecting)
      (return-from start-connection))

    (let ((socket (socket driver)))
      (set-read-callback socket
                         (lambda (data &key (start 0) end)
                           (parse driver data :start start :end end)))

      (send-handshake-response driver
                               :callback
                               (lambda ()
                                 (unless (eq (ready-state driver) :closed)
                                   (open-connection driver)))))))

(defgeneric parse (driver data &key start end))

(defgeneric send (driver data &key start end type code callback))
(defmethod send :around ((driver driver) data &key start end type code callback)
  (when (eq (ready-state driver) :connecting)
    (return-from send
      (enqueue driver (list data start end type code callback))))

  (unless (eq (ready-state driver) :open)
    (return-from send nil))

  (call-next-method))

(defgeneric send-text (driver message &key start end callback)
  (:method ((driver driver) message &rest args)
    (apply #'send driver message :type :text args)))

(defgeneric send-binary (driver message &key start end callback)
  (:method ((driver driver) message &rest args)
    (apply #'send driver message :type :binary args)))

(defgeneric send-ping (driver &optional message callback))

(defgeneric close-connection (driver &optional reason code)
  (:method ((driver driver) &optional reason code)
    (declare (ignore reason code))
    (unless (eq (ready-state driver) :open)
      (return-from close-connection))

    (setf (ready-state driver) :closed)
    (emit :close driver :code nil :reason nil)
    t))

(defgeneric open-connection (driver)
  (:method ((driver driver))
    (setf (ready-state driver) :open)

    (map nil (lambda (message)
               (destructuring-bind (message type code)
                   message
                 (send driver message :type type :code code)))
         (queue driver))

    (setf (queue driver)
          (make-array 0 :adjustable t :fill-pointer 0))

    (emit :open driver)))

(defun enqueue (driver message)
  (vector-push-extend message (queue driver))
  t)

(defgeneric send-handshake-response (driver &key callback))

(defgeneric send-handshake-request (driver &key callback))
