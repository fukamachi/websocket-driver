# Websocket Driver

[![Build Status](https://travis-ci.org/fukamachi/websocket-driver.svg?branch=master)](https://travis-ci.org/fukamachi/websocket-driver)
[![Quicklisp dist](http://quickdocs.org/badge/websocket-driver.svg)](http://quickdocs.org/websocket-driver/)

This library provides a complete implementation of the WebSocket protocols.

## Supported Servers

* [Wookie](http://wookie.lyonbros.com)
* [Woo](https://github.com/fukamachi/woo)

## Usage

### Server-side with Clack

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

(clack:clackup *echo-server* :server :wookie :port 5000)
```

### Client-side

```common-lisp
(ql:quickload :websocket-driver-client)

(defvar *client* (wsd:make-client "ws://localhost:5000/echo"))

(as:with-event-loop ()
  (wsd:start-connection *client*)
  (wsd:send *client* "Hi"))
```

## Driver API

#### `(on :open driver callback)`

Sets the `CALLBACK` function to execute when the socket becomes open.

#### `(on :message driver callback)`

Sets the `CALLBACK` to execute when a message is received. The `CALLBACK` function takes a `MESSAGE` as an argument which is either a string in the case of a text message or an `(UNSIGNED-BYTE 8)` vector in the case of a binary message.

#### `(on :error driver callback)`

Sets the `CALLBACK` to execute when a protocol error occurs due to the other peer sending an invalid byte sequence. The `CALLBACK` function takes a `PROTOCOL-ERROR` as an argument.

#### `(on :close driver callback)`

Sets the `CALLBACK` to execute when the socket becomes closed. The `CALLBACK` function takes `CODE` and `REASON` as arguments.

#### `(start-connection driver)`

Initiates the protocol by sending the handshake - either the response for a server-side driver or the request for a client-side one. This should be the first method you invoke. Returns `T` if a handshake was sent.

#### `(parse driver data)`

Takes `DATA` and parses it, potentially resulting in message events being emitted (see `(on :message ...)` above). You should send all data you receive via I/O to this method.

#### `(send driver data &key type code)`

Sends `DATA` over the socket.

#### `(send-text driver string)`

Sends a text message over the socket.

#### `(send-binary driver usb8-vector)`

Takes an `(UNSIGNED-BYTE 8)` vector and sends them as a binary message.

#### `(send-ping driver &optional message callback)`

Sends a ping frame over the socket, queueing it if necessary.

#### `(close-connection driver)`

Initiates the closing handshake if the socket is still open.

#### `(version driver)`

Returns the WebSocket version in use as a string.

#### `(protocol driver)`

Returns a string containing the selected subprotocol, if any was agreed upon using the `Sec-WebSocket-Protocol` mechanism.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.

## See Also

* [Clack](http://clacklisp.org)
* [Event Emitter](https://github.com/fukamachi/event-emitter)
* [cl-async](http://orthecreedence.github.io/cl-async/)
