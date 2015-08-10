(in-package :cl-user)
(defpackage websocket-driver-client-asd
  (:use :cl :asdf))
(in-package :websocket-driver-client-asd)

(defsystem websocket-driver-client
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:websocket-driver
               :bordeaux-threads
               :iolib)
  :components ((:module "src"
                :components
                ((:file "client" :depends-on ("driver/client"))
                 (:file "driver/client")))))
