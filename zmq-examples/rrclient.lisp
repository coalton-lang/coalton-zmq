;;;; zmq-examples/rrclient.lisp

(defpackage #:rrclient
  (:documentation "Hello World Client

Hello World client
Connects REQ socket to tcp://localhost:5559
Sends \"Hello\" to server, expects \"World\" back")
  (:use #:coalton #:coalton-prelude #:zmq #:zhelpers)
  (:import-from
   #:coalton-library/experimental/loops
   #:dotimes)
  (:import-from
   #:coalton-library/result
   #:ok?)
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:rrclient)

(coalton-toplevel

  (declare main (Unit -> Unit))
  (define (main)

    (let context = (unwrap (zmq-ctx-new)))

    ;; Socket to talk to server
    (let requester = (unwrap (zmq-socket context ZMQ-REQ)))
    (zmq-connect requester "tcp://localhost:5559")

    (catch-abort

        (dotimes (request-nbr 10)
          (s-send requester "Hello")
          (let string = (unwrap (s-recv requester)))
          (printf "~&Received reply ~D [~A]~%" request-nbr string))

      (zmq-close requester)
      (zmq-ctx-destroy context)
      (quit 0))))

(cl:defun cl-main ()
  #+SBCL (sb-ext:disable-debugger)
  (call-coalton-function load-libzmq Unit)
  (call-coalton-function main Unit))
