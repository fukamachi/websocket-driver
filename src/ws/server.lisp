(in-package :cl-user)
(defpackage websocket-driver.ws.server
  (:use :cl
        #:websocket-driver.ws.base
        #:websocket-driver.util)
  (:import-from :fast-websocket
                #:compose-frame
                #:error-code)
  (:import-from :clack.socket
                #:read-callback
                #:write-sequence-to-socket
                #:write-sequence-to-socket-buffer
                #:write-byte-to-socket-buffer
                #:flush-socket-buffer
                #:close-socket
                #:socket-async-p
                #:socket-stream)
  (:import-from :fast-io
                #:with-fast-output
                #:fast-write-sequence
                #:fast-write-byte)
  (:import-from :ironclad
                #:ascii-string-to-byte-array)
  (:import-from :trivial-utf-8
                #:string-to-utf-8-bytes)
  (:import-from #:woo.queue
                #:make-queue
                #:queue-empty-p
                #:enqueue
                #:dequeue)
  (:export #:server
           #:woo-server
           #:send-from-thread))
(in-package :websocket-driver.ws.server)

(defclass server (ws)
  ((headers :initarg :headers
            :initform (error ":headers is required")
            :type hash-table
            :accessor headers)
   (require-masking :initarg :require-masking
                    :initform t
                    :accessor require-masking)))


(defclass woo-server (server)
  ((event-loop :initform woo::*evloop*
               :reader get-event-loop)
   (async-queue :initform (make-queue)
                :reader get-queue)
   (dequeue-async :initform (cffi:foreign-alloc '(:struct lev:ev-async))
                  :reader get-dequeue-async)))


(defmethod initialize-instance :after ((server server) &key)
  (let ((protocols (accept-protocols server))
        (env-protocols (gethash "sec-websocket-protocol" (headers server))))
    (when env-protocols
      (setq env-protocols (split-by-comma env-protocols)))
    (setf (protocol server)
          (find-if (lambda (proto)
                     (find proto protocols :test #'string=))
                   env-protocols)))

  ;; Sec-Websocket-Version must be "13"
  (let ((ws-version (gethash "sec-websocket-version"
                             (headers server))))
    (etypecase ws-version
      (null
       (error "No Sec-WebSocket-Version header"))
      (string
       (unless (find "13" (split-by-comma ws-version)
                     :test #'string=)
         (error "Unsupported WebSocket version: ~S" ws-version)))
      (integer
       (unless (= ws-version 13)
         (error "Unsupported WebSocket version: ~S" ws-version)))))
  (setf (version server) "hybi-13"))


(defmethod initialize-instance :after ((server woo-server) &key)
  (lev:ev-async-init (get-dequeue-async server)
                     'send-from-main-thread)
  (lev:ev-async-start (get-event-loop server)
                      (get-dequeue-async server))
  
  ;; TODO: also, we need to put this code
  ;;       (cffi:foreign-free dequeue-async)
  ;;       somewhere, to free resource.
  )


;; TODO: I didn't find a way to access `woo-server' instance
;;       which put data into the queue, from send-from-main-thread
;;       callback.
;;       Here we probably will have a memory leak. Need advice
;;       how to solve this problem.
(defvar *listener-to-server* (make-hash-table))


(cffi:defcallback send-from-main-thread :void ((evloop :pointer) (listener :pointer) (events :int))
  (declare (ignore evloop events))
  
  (let ((server (gethash (cffi:pointer-address listener)
                         *listener-to-server*)))
    ;; Now we'll send all queued messages to the client and will
    ;; do this from the thread where the main event loop lives.
    (loop with queue = (get-queue server)
          until (queue-empty-p queue)
          for args = (dequeue queue)
          do (apply #'send server args))))


(defmethod start-connection ((server server) &key)
  (unless (eq (ready-state server) :connecting)
      (return-from start-connection))

  (let ((socket (socket server)))
    (setf (read-callback socket)
          (lambda (data &key (start 0) end)
            (parse server data :start start :end end)))

    (send-handshake-response server
                             :callback
                             (lambda ()
                               (unless (eq (ready-state server) :closed)
                                 (open-connection server))))

    (unless (clack.socket:socket-async-p socket)
      (unwind-protect
           (loop with stream = (socket-stream socket)
                 while (open-stream-p stream)
                 for frame = (read-websocket-frame stream)
                 while frame
                 do (funcall (read-callback socket) frame))
        (close-connection server)
        (setf (ready-state server) :closed)))))


(defmethod close-connection ((server server) &optional (reason "") (code (error-code :normal-closure)))
  (setf (ready-state server) :closing)
  (send server reason :type :close :code code
                      :callback
                      (let ((socket (socket server)))
                        (lambda ()
                          (setf (ready-state server) :closed)
                          (close-socket socket))))
  t)


(defmethod send ((server server) data &key start end type code callback)
  (let ((frame (compose-frame data
                              :start start
                              :end end
                              :type type
                              :code code
                              :masking nil)))
    (handler-case
	(write-sequence-to-socket (socket server) frame
                                  :callback callback)
      (error ()
        (setf (ready-state server) :closed)
	(wsd:emit :close server :code 1006 :reason "websocket connection closed")))))


;; TODO: I've made this as a separate method, but ideally, all code
;;       from `send' method should be moved to a separate function
;;       like `inner-send' and make `send' method work differently for usual
;;       `server' class and `woo-server' class.
;;       I need advice here too.
(defmethod send-from-thread ((server woo-server) data &key start end type code callback)
  (let ((queue (get-queue server)))
    (enqueue (list data
                   :start start
                   :end end
                   :type type
                   :code code
                   :callback callback)
             queue))
  (let* ((listener (get-dequeue-async server)))
    (setf (gethash (cffi:pointer-address listener)
                   *listener-to-server*)
          server)
    (lev:ev-async-send (get-event-loop server)
                       listener)))


(defmethod send-handshake-response ((server server) &key callback)
  (let ((socket (socket server))
        (sec-key (gethash "sec-websocket-key" (headers server))))
    (unless (stringp sec-key)
      (when callback (funcall callback))
      (return-from send-handshake-response))

    (labels ((octets (data)
               (write-sequence-to-socket-buffer socket data))
             (ascii-string (data)
               (octets (ascii-string-to-byte-array data)))
             (crlf ()
               (octets #.(ascii-string-to-byte-array (format nil "~C~C" #\Return #\Newline)))))
      (octets
       #.(ascii-string-to-byte-array
          (with-output-to-string (s)
            (format s "HTTP/1.1 101 Switching Protocols~C~C" #\Return #\Newline)
            (format s "Upgrade: websocket~C~C" #\Return #\Newline)
            (format s "Connection: Upgrade~C~C" #\Return #\Newline)
            (format s "Sec-WebSocket-Accept: "))))
      (ascii-string
       (generate-accept sec-key))
      (crlf)

      (let ((protocol (protocol server)))
        (when protocol
          (octets
           #.(ascii-string-to-byte-array "Sec-WebSocket-Protocol: "))
          (ascii-string protocol)
          (crlf)))

      (loop for (name . value) in (additional-headers server)
            do (ascii-string
                (string-capitalize name))
               (octets
                #.(ascii-string-to-byte-array ": "))
               (ascii-string value)
               (crlf))

      (crlf))

    (flush-socket-buffer socket :callback callback)))
