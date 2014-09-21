(in-package :cl-user)
(defpackage websocket-driver.uri
  (:use :cl)
  (:import-from :puri
                :parse-uri-string
                :uri-path
                :uri-query
                :uri-fragment)
  (:export :parse-uri))
(in-package :websocket-driver.uri)

(defun parse-uri (uri-string)
  (check-type uri-string string)
  (let ((uri (puri:parse-uri uri-string)))
    (multiple-value-bind (scheme host port path query fragment)
        (puri::parse-uri-string uri-string)
      (declare (ignore scheme host port))
      (setf (puri:uri-path uri) path
            (puri:uri-query uri) query
            (puri:uri-fragment uri) fragment))
    uri))
