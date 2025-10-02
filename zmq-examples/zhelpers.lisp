;;;; zmq-examples/zhelpers.lisp

(defpackage #:zhelpers
  (:use #:coalton #:coalton-prelude #:zmq)
  (:local-nicknames
   (#:with-user-abort #:with-user-abort))
  (:export
   #:printf
   #:sprintf
   #:catch-abort
   #:quit
   #:randof
   #:s-recv
   #:s-send))

(named-readtables:in-readtable coalton:coalton)

(in-package #:zhelpers)

(cl:defmacro printf (fmt-str cl:&rest fmt-args)
  (cl:let ((bindings (cl:list*
                      (cl:list (cl:gensym "FMT-STR") fmt-str)
                      (cl:loop :for fmt-arg :in fmt-args
                         :for fmt-var := (cl:gensym "FMT-ARG")
                         :collect (cl:list fmt-var fmt-arg)))))
    `(let ,bindings
       (lisp Unit (,@(cl:mapcar #'cl:first bindings))
         (cl:format cl:t ,@(cl:mapcar #'cl:first bindings))
         Unit))))

(cl:defmacro sprintf (fmt-str cl:&rest fmt-args)
  (cl:let ((bindings (cl:list*
                      (cl:list (cl:gensym "FMT-STR") fmt-str)
                      (cl:loop :for fmt-arg :in fmt-args
                               :for fmt-var := (cl:gensym "FMT-ARG")
                               :collect (cl:list fmt-var fmt-arg)))))
    `(let ,bindings
       (lisp String (,@(cl:mapcar #'cl:first bindings))
         (cl:format cl:nil ,@(cl:mapcar #'cl:first bindings))))))

(cl:defmacro catch-abort (form cl:&body cleanup)
  (cl:let ((act   (cl:gensym "ACT"))
           (clean (cl:gensym "CLEAN")))
    `(let ((,act (fn () ,form))
           (,clean (fn () ,@cleanup)))
       (lisp :t (,act ,clean)
         (cl:handler-case
             (with-user-abort:with-user-abort
               (call-coalton-function ,act Unit)
               (call-coalton-function ,clean Unit))
           (with-user-abort:user-abort ()
             (call-coalton-function ,clean Unit)))))))

(coalton-toplevel

  (declare quit (Int -> Unit))
  (define (quit code)
    (lisp Unit (code)
      (cl:prog1 Unit
        (uiop:quit code))))

  (declare randof (Num :t => :t -> :t))
  (define (randof num)
    (fromInt (lisp Integer (num) (cl:random num))))

  (declare s-recv (Socket -> Result ZMQError String))
  (define (s-recv socket)
    (recv socket))

  (declare s-send (Socket -> String -> Result ZMQError Unit))
  (define (s-send socket string)
    (send socket string)))

