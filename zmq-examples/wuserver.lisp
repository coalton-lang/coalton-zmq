;;;; zmq-examples/wuserver.lisp

(defpackage #:wuserver
  (:documentation "Weather Update Server

Weather update server
Binds PUB socket to tcp://5556
Publishes random weather updates")
  (:use #:coalton #:coalton-prelude #:zmq #:zhelpers)
  (:import-from
   #:coalton-library/result
   #:ok?)
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:wuserver)

(coalton-toplevel

  (declare main (Unit -> Unit))
  (define (main)

    ;; Prepare our context and publisher
    (let context = (unwrap (zmq-ctx-new)))
    (let publisher = (unwrap (zmq-socket context ZMQ-PUB)))
    (let rc = (zmq-bind publisher "tcp://*:5556"))
    (assert (ok? rc))

    (catch-abort

        (while true
          ;; Get values that will fool the boss
          (let zipcode = (randof 100000))
          (let temperature = (- (randof 215) 80))
          (let relhumidity = (+ (randof 50) 10))

          ;; Send message to all subscribers
          (let update =
            (sprintf "~5,'0D ~D ~D" zipcode temperature relhumidity))
          (s-send publisher update))

      (zmq-close publisher)
      (zmq-ctx-destroy context)
      (quit 0))))

(cl:defun cl-main ()
  #+SBCL (sb-ext:disable-debugger)
  (call-coalton-function load-libzmq Unit)
  (call-coalton-function main Unit))

