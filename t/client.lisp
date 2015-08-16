(in-package :cl-user)

(ql:quickload '(:uiop :websocket-driver-client) :silent t)

(defvar *client*
  (wsdc:make-client "ws://localhost:5000/echo"))

(wsd:on :open *client*
        (lambda ()
          (format *error-output* "~&connected~%")))

(wsd:on :message *client*
        (lambda (message)
          (if (string= message "Hi")
              (progn
                (format t "~&ok~%")
                (uiop:quit))
              (progn
                (format t "~&ng~%")
                (uiop:quit -1)))))

(as:with-event-loop (:catch-app-errors t)
  (wsd:start-connection *client*)

  (wsd:send *client* "Hi")

  (format *error-output* "~&Exiting~%"))
