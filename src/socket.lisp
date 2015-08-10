(in-package :cl-user)
(defpackage websocket-driver.socket
  (:use :cl)
  (:export :write-to-socket))
(in-package :websocket-driver.socket)

(defun write-to-socket (socket message &key callback)
  (clack.socket:write-to-socket socket message :callback callback))
