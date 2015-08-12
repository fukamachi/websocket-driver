(in-package :cl-user)
(defpackage websocket-driver
  (:nicknames :wsd)
  (:use :cl)
  (:import-from :websocket-driver.driver.hybi
                #:hybi)
  (:import-from :websocket-driver.driver.base
                #:driver
                #:socket
                #:additional-headers
                #:accept-protocols
                #:protocol
                #:version
                #:max-length
                #:ready-state

                #:start-connection
                #:send
                #:send-text
                #:send-binary
                #:send-ping
                #:close-connection)
  (:import-from #:event-emitter
                #:listeners
                #:listener-count
                #:add-listener
                #:on
                #:once
                #:remove-listener
                #:remove-all-listeners
                #:emit)
  (:export #:make-server
           #:websocket-p

           ;; from driver
           #:driver
           #:socket
           #:additional-headers
           #:accept-protocols
           #:protocol
           #:version
           #:max-length
           #:ready-state

           #:start-connection
           #:send
           #:send-text
           #:send-binary
           #:send-ping
           #:close-connection

           ;; from event-emitter
           #:listeners
           #:listener-count
           #:add-listener
           #:on
           #:once
           #:remove-listener
           #:remove-all-listeners
           #:emit))
(in-package :websocket-driver)

(defun make-server (env &rest options)
  (let ((socket (getf env :clack.io)))
    (unless socket
      (error ":clack.io doesn't exist in ENV. Probably this server is not supported."))
    (apply #'make-instance 'hybi
           :socket socket
           :headers (getf env :headers)
           options)))

(defun websocket-p (env)
  (let ((headers (getf env :headers)))
    (and (eq (getf env :request-method) :get)
         (string-equal (gethash "connection" headers "") "upgrade")
         (string-equal (gethash "upgrade" headers "") "websocket")
         (eql (gethash "sec-websocket-version" headers) 13))))
