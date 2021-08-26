(in-package :cl-user)
(defpackage websocket-driver-client-asd
  (:use :cl :asdf))
(in-package :websocket-driver-client-asd)

(defsystem websocket-driver-client
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:websocket-driver-base
               :usocket
               #-websocket-driver-no-ssl :cl+ssl
               :fast-io
               :fast-websocket
               :fast-http
               :cl-base64
               :babel
               :quri)
  :components ((:module "src"
                :components
                ((:file "ws/client")
                 (:file "client" :depends-on ("ws/client"))))))
