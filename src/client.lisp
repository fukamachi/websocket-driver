(in-package :cl-user)
(defpackage websocket-driver-client
  (:use :cl)
  (:import-from :websocket-driver.driver.client
                :client)
  (:export :make-client))
(in-package :websocket-driver-client)

(defun make-client (url &optional protocols &rest options)
  (apply #'make-instance 'client
         :url url
         :masking t
         :protocols protocols
         options))
