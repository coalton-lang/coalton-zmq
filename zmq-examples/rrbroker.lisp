;;;; zmq-examples/rrbroker.lisp

(defpackage #:rrbroker
  (:documentation "Simple request-reply broker")
  (:use #:coalton #:coalton-prelude #:zmq #:zhelpers)
  (:import-from
   #:coalton-library/result
   #:ok?)
  (:import-from
   #:coalton-library/list
   #:nth)
  (:export
   #:cl-main))

(named-readtables:in-readtable coalton:coalton)

(in-package #:rrbroker)

(coalton-toplevel

  (declare main (Unit -> Unit))
  (define (main)

    ;; Prepare our context and sockets
    (let context = (unwrap (zmq-ctx-new)))
    (let frontend = (unwrap (zmq-socket context ZMQ-ROUTER)))
    (let backend  = (unwrap (zmq-socket context ZMQ-DEALER)))
    (zmq-bind frontend "tcp://*:5559")
    (zmq-bind backend  "tcp://*:5560")

    ;; Initialize poll set
    (let (Tuple items item-list) = (new-pollitem-array 2))
    (let item0 = (nth 0 item-list))
    (let item1 = (nth 1 item-list))
    (set-pollitem-slots item0 frontend 0 ZMQ-POLLIN mempty)
    (set-pollitem-slots item1 backend  0 ZMQ-POLLIN mempty)

    ;; Switch messages between sockets
    (catch-abort

        (while True
          (let message = (new-message))
          (zmq-poll items 2 -1)
          (when (bitfield-member? ZMQ-POLLIN (pollitem-revents item0))
            (while True
              (zmq-msg-init message)
              (zmq-msg-recv message frontend mempty)
              (let more? = (zmq-msg-more message))
              (zmq-msg-send message backend (if more? ZMQ-SNDMORE mempty))
              (zmq-msg-close message)
              (when (not more?)
                (break))))
          (when (bitfield-member? ZMQ-POLLIN (pollitem-revents item1))
            (while True
              (zmq-msg-init message)
              (zmq-msg-recv message backend mempty)
              (let more? = (zmq-msg-more message))
              (zmq-msg-send message frontend (if more? ZMQ-SNDMORE mempty))
              (zmq-msg-close message)
              (when (not more?)
                (break)))))

      (free items)
      (zmq-close frontend)
      (zmq-close backend)
      (zmq-ctx-destroy context)
      (quit 0))))

(cl:defun cl-main ()
  #+SBCL (sb-ext:disable-debugger)
  (call-coalton-function load-libzmq Unit)
  (call-coalton-function main Unit))
