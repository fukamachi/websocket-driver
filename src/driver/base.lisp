(in-package :cl-user)
(defpackage websocket-driver.driver.base
  (:use :cl)
  (:import-from :event-emitter
                #:emit
                #:event-emitter)
  (:import-from :clack.socket
                #:write-to-socket
                #:set-read-callback)
  (:import-from :blackbird
                #:with-promise)
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
           #:handshake-response
           #:handshake-request))
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
                           (parse driver (subseq data start end))))

      (bb:with-promise (resolve reject)
        (write-to-socket socket
                         (handshake-response driver)
                         :callback
                         (lambda ()
                           (unless (eq (ready-state driver) :closed)
                             (open-connection driver))
                           (resolve)))))))

(defgeneric parse (driver data))

(defgeneric send (driver data &key type code))
(defmethod send :around ((driver driver) data &key type code)
  (when (eq (ready-state driver) :connecting)
    (return-from send
      (enqueue driver (list data type code))))

  (unless (eq (ready-state driver) :open)
    (return-from send nil))

  (call-next-method))

(defgeneric send-text (driver message)
  (:method ((driver driver) message)
    (send driver message)))

(defgeneric send-binary (driver message)
  (:method (driver message)
    (declare (ignore driver message))
    nil))

(defgeneric send-ping (driver &optional message callback)
  (:method (driver &optional message callback)
    (declare (ignore driver message callback))
    nil))

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

(defgeneric handshake-response (driver))

(defgeneric handshake-request (driver))
