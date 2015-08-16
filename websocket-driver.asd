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
  :depends-on (:websocket-driver-server
               :websocket-driver-client)
  :description "WebSocket protocol handler")
