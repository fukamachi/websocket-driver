(ql:quickload '(:websocket-driver :clack) :silent t)

(in-package :cl-user)
(defpackage websocket-test
  (:use :cl
        :websocket-driver))
(in-package :websocket-test)

(defvar *app*
  (lambda (env)
    (cond
      ((string= "/echo" (getf env :request-uri))
       (let ((ws (make-server env)))
         (on :message ws
             (lambda (message)
               (send ws message)))
         (lambda (responder)
           (declare (ignore responder))
           (start-connection ws))))
      (T
       '(200 (:content-type "text/html")
         ("<html>
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
"))))))

(defvar *handler*
  (clack:clackup *app* :server :wookie :use-thread nil))
