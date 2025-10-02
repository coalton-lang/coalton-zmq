;;;; zmq-examples/rrworker.lisp

(defpackage #:rrworker
  (:documentation "Hello World Worker

Hello World worker
Connects REP socket to tcp://localhost:5560
Expects \"Hello\" from client, replies with \"World\"")
  (:use #:coalton #:coalton-prelude #:zmq #:zhelpers)
  (:local-nicknames
   (#:sys #:coalton-library/system))
  (:import-from
   #:coalton-library/result
   #:ok?)
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:rrworker)

(coalton-toplevel

  (declare main (Unit -> Unit))
  (define (main)

    (let context = (unwrap (zmq-ctx-new)))

    ;; Socket to talk to clients
    (let responder = (unwrap (zmq-socket context ZMQ-REP)))
    (zmq-connect responder "tcp://localhost:5560")

    (catch-abort

        (while True

          ;; Wait for next request from client
          (let string = (unwrap (s-recv responder)))
          (printf "~&Received request: [~A]~%" string)

          ;; Do some 'work'
          (sys:sleep 1.0)

          (s-send responder "World"))

      ;; We never get here, but clean up anyhow
      (zmq-close responder)
      (zmq-ctx-destroy context)
      (quit 0))))

(cl:defun cl-main ()
  #+SBCL (sb-ext:disable-debugger)
  (call-coalton-function load-libzmq Unit)
  (call-coalton-function main Unit))

