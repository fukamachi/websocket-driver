# Websocket Driver

[![Build Status](https://travis-ci.org/fukamachi/websocket-driver.svg?branch=master)](https://travis-ci.org/fukamachi/websocket-driver)
[![Quicklisp dist](http://quickdocs.org/badge/websocket-driver.svg)](http://quickdocs.org/websocket-driver/)

This library provides WebSocket server & client implementation for Common Lisp.

## Supported Servers

* [Hunchentoot](http://weitz.de/hunchentoot/)
* [Wookie](http://wookie.lyonbros.com)
* [Woo](https://github.com/fukamachi/woo)

## Usage

### Server-side with Clack

WebSocket server implementation is designed to work with [Clack](https://github.com/fukamachi/clack), which is a abstraction layer for web servers.

```common-lisp
(ql:quickload '(:websocket-driver-server :clack))

(use-package :websocket-driver)

(defvar *echo-server*
  (lambda (env)
    (let ((ws (make-server env)))
      (on :message ws
          (lambda (message)
            (send ws message)))
      (lambda (responder)
        (declare (ignore responder))
        (start-connection ws)))))

;; Start Wookie server
(clack:clackup *echo-server* :server :wookie :port 5000)
```

The backend server can be changed by replacing `:wookie` by other servers.

### Client-side

```common-lisp
(ql:quickload :websocket-driver-client)

(defvar *client* (wsd:make-client "ws://localhost:5000/echo"))

(wsd:start-connection *client*)
(wsd:on :message *client*
        (lambda (message)
          (format t "~&Got: ~A~%" message)))
(wsd:send *client* "Hi")
```

## APIs

### \[Function] make-server (env &key max-length accept-protocols additional-headers)

Returns a new `SERVER` object. The `ENV` is a property list represents server information, which [Clack](http://clacklisp.org) provides.

The `max-length` is the maximum message size allowed. The default is `#x3ffffff`. If at any time it stays bigger than this, the connection will be closed with code 1009 (too-large).

The `accept-protocols` is a list of custom protocol names as strings. This will be used for checking `Sec-WebSocket-Protocol` header client sent. The default is an empty list.

The `additional-headers` is an association list which represents HTTP headers to use in WebSocket handshake response. The default is an empty list.

### \[Function] make-client (url &key max-length accept-protocols additional-headers)

Returns a new `CLIENT` object. The `URL` is a string to connect.

Additional keyword arguments `max-length`, `accept-protocols` and `additional-headers` are shared with `make-server`.

### \[Class] ws

The base class for `server` and `client`.

As this inherits `event-emitter`, its object can be attached event listerners by `on`.

#### \[Event] :open

Called when the socket becomes open.

```common-lisp
(on :open ws
    (lambda ()
      (format t "Connected.~%")))
```

#### \[Event] :message

Called when a message is received. The callback function takes a `MESSAGE` as an argument which is either a string in the case of a text message or an `(UNSIGNED-BYTE 8)` vector in the case of a binary message.

```common-lisp
(on :message ws
    (lambda (message)
      (format t "Received: ~S~%" message)))
```

#### \[Event] :error

Called when a protocol error occurs due to the other peer sending an invalid byte sequence. The callback function takes a `PROTOCOL-ERROR` as an argument.

```common-lisp
(on :error ws
    (lambda (error)
      (format t "Got an error: ~S~%" error)))
```

#### \[Event] :close

Called when the socket becomes closed. The `CALLBACK` function takes `CODE` and `REASON` as arguments.

```common-lisp
(on :close ws
    (lambda (code reason)
      (format t "Closed because '~A' (Code=~A)~%" reason code)))
```

### \[Class] server

The class for WebSocket (version 13) server implementation.

### \[Class] client

The class for WebSocket client implementation.

### \[Method] `(start-connection ws)`

Initiates the protocol by sending the handshake - either the response for a server-side driver or the request for a client-side one. This should be the first method you invoke. Returns `T` if a handshake was sent.

### \[Method] `(send ws data &key start end type code callback)`

Sends `DATA` over the socket.

### \[Function] `(send-text ws message &key start end callback)`

Sends a text message over the socket.

### \[Function] `(send-binary ws usb8-vector &key start end callback)`

Takes an `(UNSIGNED-BYTE 8)` vector and sends them as a binary message.

### \[Method] `(send-ping ws &optional message callback)`

Sends a ping frame over the socket, queueing it if necessary.

### \[Method] `(close-connection ws)`

Initiates the closing handshake if the socket is still open.

### \[Method] `(version driver)`

Returns the WebSocket version in use as a string (ex. "hybi-13").

### \[Method] `(protocol driver)`

Returns a string containing the selected subprotocol, if any was agreed upon using the `Sec-WebSocket-Protocol` mechanism.

### \[Method] `(ready-state ws)`

Returns the connection state as a keyword, which is one of `:connecting`, `:open`, `:closing` and `:closed`.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.

## See Also

* [Clack](http://clacklisp.org)
* [Event Emitter](https://github.com/fukamachi/event-emitter)
