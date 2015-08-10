(in-package :cl-user)
(defpackage websocket-driver.driver.base
  (:use :cl
        :annot.class
        :event-emitter
        :websocket-driver.events
        :websocket-driver.util)
  (:import-from :websocket-driver.header
                :make-headers
                :write-header)
  (:import-from :websocket-driver.socket
                :write-to-socket)
  (:import-from :cl-async-future
                :make-future
                :finish
                :attach)
  (:import-from :iolib
                :set-io-handler
                :socket-os-fd
                :receive-from)
  (:import-from :iomux
                :event-base)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-sequence)
  (:import-from :alexandria
                :define-constant))
(in-package :websocket-driver.driver.base)

(syntax:use-syntax :annot)

(define-constant +states+
  #(:connecting :open :closing :closed)
  :test 'equalp)

@export
@export-accessors
(defclass driver (event-emitter)
  ((socket :initarg :socket
           :accessor socket)
   (protocols :initarg :protocols
              :initform nil
              :accessor protocols)
   (max-length :initarg :max-length
               :initform #x3ffffff
               :accessor max-length)
   (headers :initform (make-headers)
            :accessor headers)
   (queue :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor queue)
   (stage :type fixnum
          :initform 0
          :accessor stage)
   (protocol :type list
             :initarg :protocols
             :initform nil
             :accessor protocol)
   (ready-state :type fixnum
                :initform 0)
   (event-base :initform (make-instance 'iomux:event-base :exit-when-empty t)
               :accessor event-base)))

@export
(defgeneric ready-state (driver)
  (:method ((driver driver))
    (let ((state (slot-value driver 'ready-state)))
      (cond
        ((null state) nil)
        ((and (integerp state)
              (< -1 state (length +states+)))
         (aref +states+ state))))))

@export
(defgeneric (setf ready-state) (state driver)
  (:method (state (driver driver))
    (check-type state symbol)
    (if (null state)
        (setf (slot-value driver 'ready-state) -1)
        (let ((pos (position state +states+ :test #'eq)))
          (unless pos
            (error "Invalid state: ~S" state))
          (setf (slot-value driver 'ready-state) pos)))))

@export
(defgeneric set-header (driver name value)
  (:method ((driver driver) name value)
    (unless (eq (ready-state driver) :connecting)
      (return-from set-header nil))

    (write-header (headers driver) name value)
    T))

@export
(defgeneric start-connection (driver)
  (:method ((driver driver))
    (unless (eq (ready-state driver) :connecting)
      (return-from start-connection))

    (let ((socket (socket driver))
          (future (asf:make-future)))
      (set-read-callback driver
                         (lambda (data &key (start 0) end)
                           (parse driver (subseq data start end))))

      (write-to-socket socket
                       (handshake-response driver)
                       :callback
                       (lambda () (asf:finish future)))

      (asf:attach future
                  (lambda ()
                    (unless (= (stage driver) -1)
                      (open-connection driver))))

      future)))

@export
(defgeneric version (driver))

@export
(defgeneric parse (driver data))

@export
(defgeneric send (driver data &key type code))

@export
(defgeneric send-text (driver message)
  (:method ((driver driver) message)
    (send driver message)))

@export
(defgeneric send-binary (driver message)
  (:method (driver message)
    (declare (ignore driver message))
    nil))

@export
(defgeneric send-ping (driver &optional message callback)
  (:method (driver &optional message callback)
    (declare (ignore driver message callback))
    nil))

@export
(defgeneric close-connection (driver &optional reason code)
  (:method ((driver driver) &optional reason code)
    (declare (ignore reason code))
    (unless (eq (ready-state driver) :open)
      (return-from close-connection))

    (setf (ready-state driver) :closed)
    (emit :close driver (make-close-event :code nil :reason nil))
    t))

@export
(defgeneric open-connection (driver)
  (:method ((driver driver))
    (setf (ready-state driver) :open)

    (map nil (lambda (message)
             (apply #'send driver message))
         (queue driver))

    (setf (queue driver)
          (make-array 0 :adjustable t :fill-pointer 0))

    (emit :open driver (make-open-event))))

@export
(defgeneric enqueue (driver message)
  (:method ((driver driver) message)
    (vector-push-extend message (queue driver))
    t))

@export
(defgeneric handshake-response (driver))

@export
(defgeneric handshake-request (driver))

@export
(defun set-read-callback (driver callback)
  (let* ((socket (socket driver))
         (socket-package
           (package-name (symbol-package (type-of socket)))))
    (cond
      ((string= socket-package #.(string :cl-async))
       (with-package-functions :as (socket-data (setf socket-data))
         (setf (getf (socket-data socket) :parser) callback)))
      ((string= socket-package #.(string :iolib.sockets))
       (with-package-functions :iolib (set-io-handler socket-os-fd recieve-from)
         (set-io-handler
          (event-base driver)
          (socket-os-fd socket)
          :read
          (lambda (fd event exception)
            (declare (ignore fd event exception))
            (let ((buffer-size 1024)
                  (endp nil))
              (funcall callback
                       (with-fast-output (buffer :vector)
                         (do () (endp)
                           (multiple-value-bind (data bytes-read)
                               (receive-from socket :size buffer-size)
                             (fast-write-sequence data buffer 0 bytes-read)
                             (when (< bytes-read buffer-size)
                               (setq endp t)))))))))))
      ((string= socket-package #.(string :woo.ev.socket))
       (with-package-functions :wev ((setf socket-data))
         (setf (socket-data socket) callback))))))
