#|
  This file is a part of websocket-driver project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage websocket-driver-asd
  (:use :cl :asdf))
(in-package :websocket-driver-asd)

(defsystem websocket-driver
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:fast-websocket
               :fast-io
               :clack-socket
               :event-emitter
               :trivial-utf-8
               :ironclad
               :cl-base64
               :blackbird
               :alexandria
               :split-sequence)
  :components ((:module "src"
                :components
                ((:file "driver" :depends-on ("driver-components"))
                 (:module "driver-components"
                  :pathname "driver"
                  :depends-on ("socket")
                  :components
                  ((:file "base")
                   (:file "hybi" :depends-on ("base"))))
                 (:file "socket"))))
  :description "WebSocket protocol handler")
