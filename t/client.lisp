(in-package :cl-user)

(ql:quickload '(:uiop :websocket-driver-client) :silent t)

(defvar *client*
  (wsdc:make-client "ws://localhost:5000/echo"))

(wsd:start-connection *client*)

(wsd:on :open *client*
        (lambda (socket)
          (declare (ignore socket))
          (format *error-output* "~&connected~%")))

(wsd:on :message *client*
        (lambda (event)
          (if (string= (wsd:event-data event) "Hi")
              (progn
                (format t "~&ok~%")
                (uiop:quit))
              (progn
                (format t "~&ng~%")
                (uiop:quit -1)))))

(wsd:send *client* "Hi")

(sleep 10)

(format *error-output* "Exiting")
