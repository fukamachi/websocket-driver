(in-package :cl-user)
(defpackage websocket-driver-base-asd
  (:use :cl :asdf))
(in-package :websocket-driver-base-asd)

(defsystem websocket-driver-base
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:fast-websocket
               :fast-io
               :event-emitter
               :sha1
               :split-sequence
               :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "driver" :depends-on ("ws/base"))
                 (:file "ws/base" :depends-on ("util"))
                 (:file "util"))))
  :description "WebSocket protocol handler")
