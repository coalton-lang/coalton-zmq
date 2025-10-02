;;;; zmq-examples/wuclient.lisp

(defpackage #:wuclient
  (:documentation "Weather Update Client

Weather update client
Connects SUB socket to tcp://localhost:5556
Collects weather updates and finds avg temp in zipcode")
  (:use #:coalton #:coalton-prelude #:zmq #:zhelpers)
  (:import-from
   #:coalton-library/result
   #:ok?)
  (:import-from
   #:coalton-library/experimental/loops
   #:dotimes)
  (:import-from
   #:coalton-library/math
   #:toInteger
   #:round/)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:cffi #:cffi)
   (#:list #:coalton-library/list)
   (#:str #:coalton-library/string)
   (#:sys #:coalton-library/system))
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:wuclient)

(coalton-toplevel

  (define (allocate-string str)
    (lisp Buffer (str)
      (cffi:foreign-string-alloc str)))

  (define (strlen buf)
    (lisp SizeT (buf)
      (cffi::foreign-string-length buf)))

  (define (strsplit str delimiter)
    (rec % ((acc Nil) (rem str))
      (match (str:substring-index delimiter rem)
        ((Some idx)
         (match (str:split (1+ idx) rem)
           ((Tuple first rest)
            (% (Cons first acc) rest))))
        ((None)
         (reverse (Cons rem acc))))))

  (define (dumpstr buf)
    (lisp String (buf)
      (cl:prog1 (cffi:foreign-string-to-lisp buf)
        (cffi:foreign-free buf))))

  (declare main (Unit -> Unit))
  (define (main)

    ;; Socket to talk to server
    (printf "~&Collecting updates from weather server...~%")
    (let context = (unwrap (zmq-ctx-new)))
    (let subscriber = (unwrap (zmq-socket context ZMQ-SUB)))
    (let rc = (zmq-connect subscriber "tcp://localhost:5556"))
    (assert (ok? rc))

    ;; Subscribe to server, default is NYC, 10001
    (let argv = (sys:cmd-args))
    (let argc = (length argv))
    (let filter = (allocate-string (if (> argc 0) (list:nth 0 argv) "10001 ")))
    (let rc = (zmq-setsockopt subscriber ZMQ-SUBSCRIBE filter (strlen filter)))
    (let filter = (dumpstr filter))
    (assert (ok? rc))

    ;; Process 100 updates
    (let total-temp = (cell:new 0))
    (catch-abort
        (dotimes (_update-nbr 100)
          (let string = (unwrap (s-recv subscriber)))
          (let temperature =
            (str:parse-int (list:nth 1 (strsplit string " "))))
          (cell:update! (+ (unwrap temperature)) total-temp))
      (printf "~&Average temperature for zipcode '~A' was ~,DF~%"
              filter
              (round/ (cell:read total-temp) 100))

      (zmq-close subscriber)
      (zmq-ctx-destroy context)
      (quit 0))))

(cl:defun cl-main ()
  #+SBCL (sb-ext:disable-debugger)
  (call-coalton-function load-libzmq Unit)
  (call-coalton-function main Unit))
