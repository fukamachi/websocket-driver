(in-package :cl-user)
(defpackage websocket-driver.socket
  (:use :cl
        :websocket-driver.util)
  (:export :write-to-socket))
(in-package :websocket-driver.socket)

(defun write-to-socket (socket message &key callback)
  (let ((socket-package (package-name (symbol-package (type-of socket)))))
    (cond
      ((string= socket-package #.(string :cl-async))
       (with-package-functions :as (write-socket-data)
         (write-socket-data socket message
                            :write-cb
                            (and callback
                                 (lambda (socket)
                                   (declare (ignore socket))
                                   (funcall callback))))))
      ((string= socket-package #.(string :iolib.sockets))
       (with-package-functions :iolib (send-to)
         (send-to socket message)
         (when callback
           (funcall callback))))
      ((string= socket-package #.(string :woo.ev.socket))
       (with-package-functions :woo.ev.socket (write-socket-data (setf socket-write-cb) socket-write-watcher)
         (with-package-functions :lev (ev-io-start)
           (write-socket-data socket message)
           (setf (socket-write-cb socket) (and callback
                                               (lambda (socket)
                                                 (declare (ignore socket))
                                                 (funcall callback))))
           (ev-io-start (symbol-value (intern #.(string :*evloop*) :woo.ev.event-loop))
                        (socket-write-watcher socket))))))
    (values)))
