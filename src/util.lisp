(in-package :cl-user)
(defpackage websocket-driver.util
  (:use :cl)
  (:export :with-package-functions))
(in-package :websocket-driver.util)

(defmacro with-package-functions (package-designator functions &body body)
  (let ((args (gensym "ARGS"))
        (g-pkg (gensym "PACKAGE")))
    `(let ((,g-pkg ,package-designator))
       (flet (,@(loop for fn in functions
                      collect `(,fn (&rest ,args)
                                    (apply
                                     ,(if (and (listp fn) (eq (car fn) 'setf))
                                          `(eval `(function (setf ,(intern ,(string (cadr fn)) ,g-pkg))))
                                          `(symbol-function (intern ,(string fn) ,g-pkg)))
                                     ,args))))
         ,@body))))
