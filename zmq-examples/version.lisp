;;;; zmq-examples/version.lisp

(defpackage #:version
  (:use #:coalton #:coalton-prelude #:zmq)
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:version)

(coalton-toplevel

  (define (quit code)
    (lisp Unit (code) (cl:prog1 Unit (uiop:quit code))))

  (declare main (Unit -> Unit))
  (define (main)
    (load-libzmq)
    (let version = (zmq-version))
    (print (mconcat (make-list "Current 0MQ version is " (into version))))
    (quit 0)))

(cl:defun cl-main () (call-coalton-function main Unit))
