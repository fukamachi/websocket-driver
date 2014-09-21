(in-package :cl-user)
(defpackage websocket-driver.error
  (:use :cl)
  (:import-from :alexandria
                :plist-hash-table
                :hash-table-values)
  (:export :protocol-error
           :too-large
           :unacceptable
           :encoding-error
           :error-code
           :valid-error-code-p))
(in-package :websocket-driver.error)

(define-condition protocol-error (simple-error) ())

(define-condition too-large (protocol-error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "WebSocket frame length too large"))))

(define-condition unacceptable (protocol-error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Received unmasked frame but masking is required"))))

(define-condition encoding-error (protocol-error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Could not decode a text frame as UTF-8"))))

(defparameter *error-codes-map*
  (plist-hash-table '(:normal-closure       1000
                      :going-away           1001
                      :protocol-error       1002
                      :unacceptable         1003
                      :encoding-error       1007
                      :policy-violation     1008
                      :too-large            1009
                      :extension-error      1010
                      :unexpected-condition 1011)
                    :test 'eq))

(defparameter *error-codes*
  (hash-table-values *error-codes-map*))

(defun error-code (error)
  (etypecase error
    (keyword (gethash error *error-codes-map*))
    (protocol-error
     (error-code (intern (string (class-name (class-of error))) :keyword)))))

(defun valid-error-code-p (code)
  (if (find code *error-codes* :test #'=) t nil))
