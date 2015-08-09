(in-package :cl-user)
(defpackage websocket-driver.socket
  (:use :cl)
  (:import-from :cl-async
                :write-socket-data)
  (:import-from :iolib
                :send-to)
  (:export :write-to-socket))
(in-package :websocket-driver.socket)

(defgeneric write-to-socket (socket message &key callback)
  (:method ((socket as:socket) message &key callback)
    (as:write-socket-data socket message
                          :write-cb
                          (and callback
                               (lambda (socket)
                                 (declare (ignore socket))
                                 (funcall callback))))
    (values))
  (:method ((socket iolib:socket) message &key callback)
    (iolib:send-to socket message)
    (and callback
         (funcall callback))
    (values))
  (:method ((socket wev:socket) message &key callback)
    (wev:with-async-writing (socket :write-cb (and callback
                                                   (lambda (socket)
                                                     (declare (ignore socket))
                                                     (funcall callback))))
      (wev:write-socket-data socket message))
    (values)))
