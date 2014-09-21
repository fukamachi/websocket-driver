(ql:quickload '(:websocket-driver :wookie))

(in-package :cl-user)
(defpackage websocket-test
  (:use :cl
        :wookie
        :websocket-driver))
(in-package :websocket-test)

(defroute (:get "/") (req res)
  (declare (ignore req))
  (send-response res :body "<html>
  <head>
    <script type=\"text/javascript\">
      var ws = null;
      function connect() {
        ws = new WebSocket(\"ws://localhost:5000/echo\");
        ws.onmessage = function(evt) { console.log(evt.data); };
      }
      function send(message) {
        ws.send(message)
      }
    </script>
  </head>
  <body>
    Open JavaScript console.
  </body>
</html>
"))

(defroute (:get "/echo" :chunk nil) (req res)
  (declare (ignore res))
  (let ((ws (make-server req nil :type :wookie)))
    (on :message ws
        (lambda (event)
          (send ws (event-data event))))
    (start-connection ws)))

(as:with-event-loop ()
  (start-server (make-instance 'listener :port 5000)))
