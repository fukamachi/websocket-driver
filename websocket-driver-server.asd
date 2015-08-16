(in-package :cl-user)
(defpackage websocket-driver-server-asd
  (:use :cl :asdf))
(in-package :websocket-driver-server-asd)

(defsystem websocket-driver-server
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:websocket-driver-base
               :fast-websocket
               :clack-socket
               :fast-io
               :ironclad
               :trivial-utf-8)
  :components ((:module "src"
                :components
                ((:file "server" :depends-on ("driver/hybi"))
                 (:file "driver/hybi"))))
  :description "WebSocket protocol handler")
