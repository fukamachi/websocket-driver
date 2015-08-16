(in-package :cl-user)
(defpackage websocket-driver-client
  (:nicknames :wsdc)
  (:use :cl)
  (:import-from :websocket-driver.driver.client
                #:client)
  (:export #:make-client))
(in-package :websocket-driver-client)

(defun make-client (url &rest options)
  (apply #'make-instance 'client
         :url url
         options))
