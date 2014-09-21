(in-package :cl-user)
(defpackage websocket-driver.events
  (:use :cl
        :annot.class))
(in-package :websocket-driver.events)

(syntax:use-syntax :annot)

@export
(deftype event ()
  '(or
    connect-event
    open-event
    message-event
    close-event))

@export
@export-constructors
(defstruct connect-event)

@export
@export-constructors
(defstruct open-event)

@export
@export-constructors
@export-accessors
(defstruct (message-event (:conc-name #.(string :event-)))
  data)

@export
@export-constructors
@export-accessors
(defstruct (close-event (:conc-name #.(string :event-)))
  code
  reason)
