;;;; zmq-examples/hwclient.lisp

(defpackage #:hwclient
  (:use #:coalton #:coalton-prelude #:zmq)
  (:import-from
   #:coalton-library/experimental/loops
   #:dotimes)
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:hwclient)

(coalton-toplevel

  (define (allocate-char-buffer len)
    (lisp Buffer (len) (cffi:foreign-alloc ':char :count len)))

  (define (allocate-string str)
    (lisp Buffer (str) (cffi:foreign-string-alloc str)))

  (define (quit code)
    (lisp Unit (code) (cl:prog1 Unit (uiop:quit code))))

  (declare main (Unit -> Unit))
  (define (main)

    (print "Connecting to hello world server...")
    (let context = (unwrap (zmq-ctx-new)))
    (let requester = (unwrap (zmq-socket context ZMQ-REQ)))
    (zmq-connect requester "tcp://localhost:5555")

    (dotimes (request-nbr 10)
      (let buffer = (allocate-char-buffer 10))
      (print (mconcat (make-list "Sending Hello " (into request-nbr) "...")))
      (let k-request-string = (allocate-string "Hello"))
      (zmq-send requester k-request-string 5 mempty)
      (free k-request-string)
      (zmq-recv requester buffer 10 mempty)
      (free buffer)
      (print (mconcat (make-list "Received World " (into request-nbr)))))
    (zmq-close requester)
    (zmq-ctx-destroy context)
    (quit 0)))

(cl:defun cl-main ()
  #+SBCL (sb-ext:disable-debugger)
  (call-coalton-function load-libzmq Unit)
  (call-coalton-function main Unit))
