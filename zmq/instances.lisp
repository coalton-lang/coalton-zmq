;;;; zmq/instances.lisp

(named-readtables:in-readtable coalton:coalton)

(in-package #:zmq)

(coalton-toplevel

  ;; STRING

  (define-instance (Into String Message)
    (define (into string)
      (let message = (new-message))
      (let size = (fromInt (into (string:length string))))
      (zmq-msg-init-size message size)
      (let buffer = (zmq-msg-data message))
      (lisp Unit (string buffer size)
        (cl:prog1 Unit
          (cffi:lisp-string-to-foreign string buffer (cl:1+ size))))
      message))

  (define-instance (Into Message String)
    (define (into message)
      (let size = (zmq-msg-size message))
      (let buffer = (zmq-msg-data message))
      (lisp String (buffer size)
        (cffi:foreign-string-to-lisp buffer :count size))))

  (define-instance (Into String (List Message))
    (define (into x)
      (singleton (into x))))

  (define-instance (Into (List Message) String)
    (define (into x)
      (mconcatmap into x)))

  (define-instance (Into (List String) (List Message))
    (define (into x)
      (map into x)))

  (define-instance (Into (List Message) (List String))
    (define (into x)
      (map into x)))

  ;; OCTETS

  (define-type-alias Array array:LispArray)
  (define-type-alias Octet U8)
  (define-type-alias Octets (Array Octet))

  (define-instance (Into Octets Message)
    (define (into octets)
      (let message = (new-message))
      (let size = (fromInt (into (array:length octets))))
      (zmq-msg-init-size message size)
      (let buffer = (zmq-msg-data message))
      (lisp Unit (buffer size octets)
        (cl:prog1 Unit
          (cffi:with-pointer-to-vector-data (octets* octets)
            (zmq-cffi:memcpy buffer octets* size))))
      message))

  (define-instance (Into Message Octets)
    (define (into message)
      (let size = (fromInt (into (zmq-msg-size message))))
      (let buffer = (zmq-msg-data message))
      (let octets = (array:make-uninitialized size))
      (lisp Unit (buffer size octets)
        (cl:prog1 Unit
          (cffi:with-pointer-to-vector-data (octets* octets)
            (zmq-cffi:memcpy octets* buffer size))))
      octets))

  (define-instance (Into Octets (List Message))
    (define (into x)
      (singleton (into x))))

  (define-instance (Into (List Message) Octets)
    (define (into x)
      (let x = (map (as Octets) x))
      (lisp Octets (x)
        (cl:let ((type '(cl:simple-array (cl:unsigned-byte 8) (cl:*))))
          (cl:apply #'cl:concatenate type x)))))

  (define-instance (Into (List Octets) (List Message))
    (define (into x)
      (map into x)))

  (define-instance (Into (List Message) (List Octets))
    (define (into x)
      (map into x))))

;; PRIMITIVES

(cl:defmacro define-primitive-message (coalton-type cffi-type)
  (cl:let ((x (cl:gensym "X"))
           (message (cl:gensym "MESSAGE"))
           (size (cl:gensym "SIZE"))
           (buffer (cl:gensym "BUFFER")))
    `(coalton-toplevel

       (define-instance (Into ,coalton-type Message)
         (define (into ,x)
           (let ,message = (new-message))
           (let ,size = ,(cffi:foreign-type-size cffi-type))
           (zmq-msg-init-size ,message ,size)
           (let ,buffer = (zmq-msg-data ,message))
           (lisp Unit (,buffer ,x)
             (cl:prog1 Unit
               (cl:setf (cffi:mem-ref ,buffer ,cffi-type) ,x)))
           ,message))

       (define-instance (Into Message ,coalton-type)
         (define (into ,message)
           (let ,buffer = (zmq-msg-data ,message))
           (lisp ,coalton-type (,buffer)
             (cffi:mem-ref ,buffer ,cffi-type))))

       (define-instance (Into ,coalton-type (List Message))
         (define (into ,x)
           (singleton (into ,x))))

       (define-instance (Into (List ,coalton-type) (List Message))
         (define (into ,x)
           (map into ,x)))

       (define-instance (Into (List Message) (List ,coalton-type))
         (define (into ,x)
           (map into ,x))))))

(define-primitive-message U8  :uint8)
(define-primitive-message U16 :uint16)
(define-primitive-message U32 :uint32)
(define-primitive-message U64 :uint64)

(define-primitive-message I8  :int8)
(define-primitive-message I16 :int16)
(define-primitive-message I32 :int32)
(define-primitive-message I64 :int64)

(define-primitive-message F32  :float)
(define-primitive-message F64  :double)

(define-primitive-message Boolean :bool)

