;;;; zmq/functions.lisp

(named-readtables:in-readtable coalton:coalton)

(in-package #:zmq)

(coalton-toplevel

  (declare load-libzmq (Unit -> Unit))
  (define (load-libzmq)
    (lisp Unit ()
      (cl:prog1 Unit
        (cffi:load-foreign-library 'zmq-cffi:libzmq))))

  (declare zmq-errno (Unit -> ZMQError))
  (define (zmq-errno)
    (lisp ZMQError ()
      (zmq-cffi:zmq-errno)))

  (declare zmq-strerror (ZMQError -> String))
  (define (zmq-strerror errnum)
    (lisp String (errnum)
      (zmq-cffi:zmq-strerror errnum)))

  (declare zmq-version (Unit -> Version))
  (define (zmq-version)
    (lisp Version ()
      (cffi:with-foreign-objects ((major :int 1)
                                  (minor :int 1)
                                  (patch :int 1))
        (zmq-cffi:zmq-version major minor patch)
        (Version (cffi:mem-ref major :int)
                 (cffi:mem-ref minor :int)
                 (cffi:mem-ref patch :int)))))

  (declare zmq-ctx-new (Unit -> Optional Context))
  (define (zmq-ctx-new)
    (lisp (Optional Context) ()
      (optionalize-pointer
       (zmq-cffi:zmq-ctx-new))))

  (declare zmq-ctx-term (Context -> Result ZMQError Unit))
  (define (zmq-ctx-term context)
    (lisp (Result ZMQError Unit) (context)
      (cl:if (cl:= -1 (zmq-cffi:zmq-ctx-term context))
             (Err (zmq-cffi:zmq-errno))
             (Ok Unit))))

  (declare zmq-ctx-shutdown (Context -> Result ZMQError Unit))
  (define (zmq-ctx-shutdown context)
    (lisp (Result ZMQError Unit) (context)
      (cl:if (cl:= -1 (zmq-cffi:zmq-ctx-shutdown context))
             (Err (zmq-cffi:zmq-errno))
             (Ok Unit))))

  (declare zmq-ctx-set (Context -> ContextOption -> Int -> Result ZMQError Unit))
  (define (zmq-ctx-set context option optval)
    (lisp (Result ZMQError Unit) (context option optval)
      (cl:if (cl:= -1 (zmq-cffi:zmq-ctx-set context option optval))
             (Err (zmq-cffi:zmq-errno))
             (Ok Unit))))

  (declare zmq-ctx-get (Context -> ContextOption -> Result ZMQError Int))
  (define (zmq-ctx-get context option)
    (lisp (Result ZMQError Int) (context option)
      (cl:let ((ret (zmq-cffi:zmq-ctx-get context option)))
        (cl:if (cl:= -1 ret)
               (Err (zmq-cffi:zmq-errno))
               (Ok ret)))))

  (declare zmq-init (Int -> Optional Context))
  (define (zmq-init io-threads)
    (lisp (Optional Context) (io-threads)
      (optionalize-pointer
       (zmq-cffi:zmq-init io-threads))))

  (declare zmq-term (Context -> Result ZMQError Unit))
  (define (zmq-term context)
    (lisp (Result ZMQError Unit) (context)
      (cl:if (cl:= -1 (zmq-cffi:zmq-term context))
             (Err (zmq-cffi:zmq-errno))
             (Ok Unit))))

  (declare zmq-ctx-destroy (Context -> Result ZMQError Unit))
  (define (zmq-ctx-destroy context)
    (lisp (Result ZMQError Unit) (context)
      (cl:if (cl:= -1 (zmq-cffi:zmq-ctx-destroy context))
             (Err (zmq-cffi:zmq-errno))
             (Ok Unit))))

  ;; Message.

  (declare zmq-msg-init (Message -> Unit))
  (define (zmq-msg-init msg)
    (lisp Unit (msg)
      (cl:prog1 Unit
        (zmq-cffi:zmq-msg-init msg))))

  (declare zmq-msg-init-size (Message -> SizeT -> Result ZMQError Unit))
  (define (zmq-msg-init-size msg size)
    (lisp (Result ZMQError Unit) (msg size)
      (cl:if (cl:= -1 (zmq-cffi:zmq-msg-init-size msg size))
             (Err (zmq-cffi:zmq-errno))
             (Ok Unit))))

  (declare zmq-msg-init-data
    (Message -> Buffer -> SizeT -> Callback -> Buffer -> Result ZMQError Unit))
  (define (zmq-msg-init-data msg data size ffn hint)
    (lisp (Result ZMQError Unit) (msg data size ffn hint)
      (cl:if (cl:= -1 (zmq-cffi:zmq-msg-init-data msg data size ffn hint))
             (Err (zmq-cffi:zmq-errno))
             (Ok Unit))))

  (declare zmq-msg-send (Message -> Socket -> SendRecvFlag -> Result ZMQError Int))
  (define (zmq-msg-send msg s flags)
    (lisp (Result ZMQError Int) (msg s flags)
      (cl:let ((ret (zmq-cffi:zmq-msg-send msg s flags)))
        (cl:if (cl:= -1 ret)
               (Err (zmq-cffi:zmq-errno))
               (Ok ret)))))

  (declare zmq-msg-recv (Message -> Socket -> SendRecvFlag -> Result ZMQError Int))
  (define (zmq-msg-recv msg s flags)
    (lisp (Result ZMQError Int) (msg s flags)
      (cl:let ((ret (zmq-cffi:zmq-msg-recv msg s flags)))
        (cl:if (cl:= -1 ret)
               (Err (zmq-cffi:zmq-errno))
               (Ok ret)))))

  (declare zmq-msg-close (Message -> Result ZMQError Unit))
  (define (zmq-msg-close msg)
    (lisp (Result ZMQError Unit) (msg)
      (unit-or-errno
       (zmq-cffi:zmq-msg-close msg))))

  (declare zmq-msg-move (Message -> Message -> Result ZMQError Unit))
  (define (zmq-msg-move dest src)
    (lisp (Result ZMQError Unit) (dest src)
      (unit-or-errno
       (zmq-cffi:zmq-msg-move dest src))))

  (declare zmq-msg-copy (Message -> Message -> Result ZMQError Unit))
  (define (zmq-msg-copy dest src)
    (lisp (Result ZMQError Unit) (dest src)
      (unit-or-errno
       (zmq-cffi:zmq-msg-copy dest src))))

  (declare zmq-msg-data (Message -> Buffer))
  (define (zmq-msg-data msg)
    (lisp Buffer (msg)
      (zmq-cffi:zmq-msg-data msg)))

  (declare zmq-msg-size (Message -> SizeT))
  (define (zmq-msg-size msg)
    (lisp SizeT (msg)
      (zmq-cffi:zmq-msg-size msg)))

  (declare zmq-msg-more (Message -> Boolean))
  (define (zmq-msg-more msg)
    (nonzero?
     (lisp Int (msg)
       (zmq-cffi:zmq-msg-more msg))))

  (declare zmq-msg-get (Message -> MessageOption -> Result ZMQError Int))
  (define (zmq-msg-get msg property)
    (lisp (Result ZMQError Int) (msg property)
      (return-or-errno
       (zmq-cffi:zmq-msg-get msg property))))

  (declare zmq-msg-set (Message -> MessageOption -> Int -> Result ZMQError Unit))
  (define (zmq-msg-set msg property optval)
    (lisp (Result ZMQError Unit) (msg property optval)
      (unit-or-errno
       (zmq-cffi:zmq-msg-set msg property optval))))

  (declare zmq-msg-gets (Message -> MessageOption -> Result ZMQError Buffer))
  (define (zmq-msg-gets msg property)
    (lisp (Result ZMQError Buffer) (msg property)
      (cl:let ((ret (zmq-cffi:zmq-msg-gets msg property)))
        (cl:if (cffi:null-pointer-p ret)
               (Err (zmq-cffi:zmq-errno))
               (Ok ret)))))

  ;; Sockets.

  (declare zmq-socket (Context -> SocketType -> Optional Socket))
  (define (zmq-socket ctx type)
    (lisp (Optional Socket) (ctx type)
      (optionalize-pointer
       (zmq-cffi:zmq-socket ctx type))))

  (declare zmq-close (Socket -> Result ZMQError Unit))
  (define (zmq-close s)
    (lisp (Result ZMQError Unit) (s)
      (unit-or-errno
       (zmq-cffi:zmq-close s))))

  (declare zmq-setsockopt (Socket -> SocketOption -> Buffer -> SizeT -> Result ZMQError Unit))
  (define (zmq-setsockopt s option optval optvallen)
    (lisp (Result ZMQError Unit) (s option optval optvallen)
      (unit-or-errno
       (zmq-cffi:zmq-setsockopt s option optval optvallen))))

  (declare zmq-getsockopt (Socket -> SocketOption -> Buffer -> SizeT -> Result ZMQError Unit))
  (define (zmq-getsockopt s option optval optvallen)
    (lisp (Result ZMQError Unit) (s option optval optvallen)
      (unit-or-errno
       (zmq-cffi:zmq-getsockopt s option optval optvallen))))

  (declare zmq-bind (Socket -> String -> Result ZMQError Unit))
  (define (zmq-bind s addr)
    (lisp (Result ZMQError Unit) (s addr)
      (unit-or-errno
       (zmq-cffi:zmq-bind s addr))))

  (declare zmq-connect (Socket -> String -> Result ZMQError Unit))
  (define (zmq-connect s addr)
    (lisp (Result ZMQError Unit) (s addr)
      (unit-or-errno
       (zmq-cffi:zmq-connect s addr))))

  (declare zmq-unbind (Socket -> String -> Result ZMQError Unit))
  (define (zmq-unbind s addr)
    (lisp (Result ZMQError Unit) (s addr)
      (unit-or-errno
       (zmq-cffi:zmq-unbind s addr))))

  (declare zmq-disconnect (Socket -> String -> Result ZMQError Unit))
  (define (zmq-disconnect s addr)
    (lisp (Result ZMQError Unit) (s addr)
      (unit-or-errno
       (zmq-cffi:zmq-disconnect s addr))))

  (declare zmq-send (Socket -> Buffer -> SizeT -> SendRecvFlag -> Result ZMQError Int))
  (define (zmq-send s buf len flags)
    (lisp (Result ZMQError Int) (s buf len flags)
      (return-or-errno
       (zmq-cffi:zmq-send s buf len flags))))
  
  (declare zmq-send-const (Socket -> Buffer -> SizeT -> SendRecvFlag -> Result ZMQError Int))
  (define (zmq-send-const s buf len flags)
    (lisp (Result ZMQError Int) (s buf len flags)
      (return-or-errno
       (zmq-cffi:zmq-send-const s buf len flags))))
  
  (declare zmq-recv (Socket -> Buffer -> SizeT -> SendRecvFlag -> Result ZMQError Int))
  (define (zmq-recv s buf len flags)
    (lisp (Result ZMQError Int) (s buf len flags)
      (return-or-errno
       (zmq-cffi:zmq-recv s buf len flags))))

  (declare zmq-socket-monitor
           (Socket -> String -> SocketTransportEvent -> Result ZMQError Int))
  (define (zmq-socket-monitor s addr events)
    (lisp (Result ZMQError Int) (s addr events)
      (return-or-errno
       (zmq-cffi:zmq-socket-monitor s addr events))))

  (declare zmq-poll (Pollitem -> Int -> Long -> Result ZMQError Int))
  (define (zmq-poll items nitems timeout)
    (lisp (Result ZMQError Int) (items nitems timeout)
      (return-or-errno
       (zmq-cffi:zmq-poll items nitems timeout)))))

