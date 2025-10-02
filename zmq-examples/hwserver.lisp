;;;; zmq-examples/hwserver.lisp

(defpackage #:hwserver
  (:use #:coalton #:coalton-prelude #:zmq)
  (:import-from
   #:coalton-library/system
   #:sleep)
  (:import-from
   #:coalton-library/result
   #:ok?)
  (:local-nicknames
   (#:cffi #:cffi))
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:hwserver)

(coalton-toplevel

  (define (allocate-char-buffer len)
    (lisp Buffer (len) (cffi:foreign-alloc ':char :count len)))

  (define (allocate-string str)
    (lisp Buffer (str) (cffi:foreign-string-alloc str)))

  (define (str-sizeof buf)
    (lisp SizeT (buf) (cffi::foreign-string-length buf)))

  (define (quit code)
    (lisp Unit (code) (cl:prog1 Unit (uiop:quit code))))

  (declare main (Unit -> Unit))
  (define (main)

    (let context = (unwrap (zmq-ctx-new)))
    (let responder = (unwrap (zmq-socket context ZMQ-REP)))
    (let response = (zmq-bind responder "tcp://*:5555"))
    (assert (ok? response))

    (while True
      (let k-read-buffer-length = (the SizeT 10))
      (let buffer = (allocate-char-buffer k-read-buffer-length))
      (zmq-recv responder buffer k-read-buffer-length mempty)
      (free buffer)
      (print "Received Hello")
      (sleep 1.0)
      (let k-reply-string = (allocate-string "World"))
      (zmq-send responder k-reply-string (str-sizeof k-reply-string) mempty)
      (free k-reply-string))

    (quit 0)))

(cl:defun cl-main ()
  #+SBCL (sb-ext:disable-debugger)
  (call-coalton-function load-libzmq Unit)
  (call-coalton-function main Unit))

