(ql:quickload '(:websocket-driver :clack) :silent t)

(in-package :cl-user)
(defpackage websocket-test
  (:use :cl
        :websocket-driver))
(in-package :websocket-test)

(defun server-http-handler (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, Clack!")))

(defun server-socket-handler (ws)
  (wsd:on :message ws
          (lambda (message)
            (websocket-driver:send ws message)))
  (lambda (responder)
    (declare (ignore responder))
    (wsd:start-connection ws)))


(defparameter *handler* (clack:clackup  (lambda (env)
                                          (let ((ws (wsd:make-server env)))
                                            (if ws
                                                (server-socket-handler ws)
                                                (server-http-handler env))))
                                        :server :woo
                                        :port 5000))
