(in-package :cl-user)
(defpackage websocket-driver
  (:nicknames :wsd)
  (:use :cl
        :cl-reexport)
  (:import-from :websocket-driver.driver.hybi
                :hybi))
(in-package :websocket-driver)

(syntax:use-syntax :annot)

@export
(defun make-server (env &rest options &key socket &allow-other-keys)
  (apply #'make-instance 'hybi
         :socket (or socket
                     (getf env :clack.io))
         :env env
         :require-masking t
         options))

@export
(defun websocket-p (env)
  (let ((headers (getf env :headers)))
    (and (eq (getf env :request-method) :get)
         (ppcre:scan "(?i)(?:^|\\s|,)upgrade(?:$|\\s|,)" (gethash "connection" headers ""))
         (string-equal (gethash "upgrade" headers) "websocket"))))

(reexport-from :websocket-driver.driver.base
               :include '(:driver
                          :ready-state
                          :set-header
                          :start-connection
                          :version
                          :protocol
                          :parse
                          :send
                          :send-text
                          :send-binary
                          :send-ping
                          :close-connection))

(reexport-from :event-emitter
               :include '(:listeners
                          :listener-count
                          :add-listener
                          :on
                          :once
                          :remove-listener
                          :remove-all-listeners
                          :emit))
