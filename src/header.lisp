(in-package :cl-user)
(defpackage websocket-driver.header
  (:use :cl
        :annot.class)
  (:import-from :fast-io
                :make-output-buffer
                :finish-output-buffer
                :fast-write-byte
                :fast-write-sequence)
  (:import-from :babel
                :string-to-octets))
(in-package :websocket-driver.header)

(syntax:use-syntax :annot)

(defparameter *clrf-octets*
  (babel:string-to-octets (format nil "~C~C" #\Return #\Linefeed)))

(defparameter *colon-byte* 58)
(defparameter *space-byte* 32)

@export
@export-constructors
(defstruct headers
  (%buffer (fast-io::make-output-buffer)))

@export
(defun write-header (headers name value)
  (check-type headers headers)
  (check-type name string)
  (check-type value (or string (unsigned-byte 8)))
  (let ((buffer (headers-%buffer headers)))
    (fast-write-sequence (babel:string-to-octets name) buffer)
    (fast-write-byte *colon-byte* buffer)
    (fast-write-byte *space-byte* buffer)
    (fast-write-sequence (if (stringp value)
                             (babel:string-to-octets value)
                             value)
                         buffer)
    (fast-write-sequence *clrf-octets* buffer)))

@export
(defun finalize-headers (headers)
  (check-type headers headers)
  (fast-io::finish-output-buffer (headers-%buffer headers)))
