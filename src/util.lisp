(in-package :cl-user)
(defpackage websocket-driver.util
  (:use :cl)
  (:import-from :split-sequence
                #:split-sequence)
  (:export #:split-by-comma
           #:generate-accept))
(in-package :websocket-driver.util)

(defparameter +guid+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defun split-by-comma (string)
  (mapl (lambda (parts)
          (rplaca parts (string-trim '(#\Space) (car parts))))
        (split-sequence #\, string)))

(defun generate-accept (key)
  (declare (optimize (speed 3) (safety 0))
           (type simple-string key))
  (sha1:sha1-base64 (concatenate 'string key +guid+)))
