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
  :version "0.0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:fast-io
               :babel
               :puri
               :cl-base64
               :cl-ppcre
               :ironclad
               :clack-socket
               :event-emitter
               :cl-async-future
               :iolib
               :bordeaux-threads
               :alexandria
               :cl-syntax-annot
               :cl-reexport)
  :components ((:module "src"
                :components
                ((:file "driver" :depends-on ("driver-components"
                                              "error"
                                              "events"
                                              "util"))
                 (:module "driver-components"
                  :pathname "driver"
                  :depends-on ("events" "socket" "uri" "header" "error" "util")
                  :components
                  ((:file "base")
                   (:file "hybi" :depends-on ("base"))
                   (:file "client" :depends-on ("base" "hybi"))))
                 (:file "events")
                 (:file "error")
                 (:file "socket" :depends-on ("util"))
                 (:file "header")
                 (:file "uri")
                 (:file "util"))))
  :description "WebSocket protocol handler")
