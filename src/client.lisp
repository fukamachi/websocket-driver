(in-package :cl-user)
(defpackage websocket-driver-client
  (:use :cl)
  (:import-from :websocket-driver.ws.client
                #:client)
  (:export #:make-client))
(in-package :websocket-driver-client)

(defun make-client (url &rest options)
  (apply #'make-instance 'client
         :url url
         options))

(import 'make-client :websocket-driver)
(export 'make-client :websocket-driver)
