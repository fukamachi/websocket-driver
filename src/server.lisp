(in-package :cl-user)
(defpackage websocket-driver.server
  (:use :cl)
  (:import-from :websocket-driver.ws.server
                #:server)
  (:export #:make-server))
(in-package :websocket-driver.server)

(defun upgrade-p (env)
  "differntiate between html and socket requests."
  (if (equal (getf env :request-method) :get)
      (let ((headers (getf env :headers)))
        (if (equal (gethash "connection" headers) "Upgrade")
            (if (equal (gethash "upgrade" headers) "websocket")
                t)))))

(defun make-server (env &rest options &key max-length accept-protocols additional-headers)
  (declare (ignore max-length accept-protocols additional-headers))
  (if (upgrade-p env)
      (let ((socket (getf env :clack.io)))
        (unless socket
          (error ":clack.io doesn't exist in ENV. Probably this server is not supported."))
        (apply #'make-instance 'server
               :socket socket
               :headers (getf env :headers)
               options))))

(import 'make-server :websocket-driver)
(export 'make-server :websocket-driver)
