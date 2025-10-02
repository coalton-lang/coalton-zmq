;;;; zmq/utilities.lisp

(named-readtables:in-readtable coalton:coalton)

(in-package #:zmq)

(cl:defmacro define-zmq-union (openp type cl:&body members)
  (cl:let ((member-values
             (cl:loop
                :for member :in members
                :for name := (cl:symbol-name member)
                :for cl-member := (cl:find-symbol name '#:zmq-cffi)
                :collect (cl:symbol-value cl-member))))
    `(progn (repr :native ,(cl:if openp
                                 'cl:t
                                 `(cl:member ,@member-values)))
            (define-type ,type)
            ,@(cl:loop
                 :for member :in members
                 :for member-value :in member-values
                 :append `((declare ,member ,type)
                           (define ,member
                             (lisp ,type () ,member-value)))))))

(cl:defmacro define-zmq-closed-union (type cl:&body members)
  `(define-zmq-union cl:nil ,type ,@members))

(cl:defmacro define-zmq-open-union (type cl:&body members)
  `(define-zmq-union cl:t ,type ,@members))

(cl:defmacro define-zmq-bitfield (type cl:&body members)
  (cl:let* ((member-values
              (cl:loop
                 :for member :in members
                 :for name := (cl:symbol-name member)
                 :for cl-member := (cl:find-symbol name '#:zmq-cffi)
                 :collect (cl:symbol-value cl-member)))
            (nbits
              (cl:integer-length
               (cl:reduce #'cl:logior member-values)))
            (x (cl:gensym "X"))
            (y (cl:gensym "Y")))
    `(progn (repr :native (cl:unsigned-byte ,nbits))
            (define-type ,type)
            (define-instance (Eq ,type)
              (inline)
              (define (== ,x ,y)
                (lisp Boolean (,x ,y) (cl:= ,x ,y))))
            (define-instance (SemiGroup ,type)
              (inline)
              (define (<> ,x ,y)
                (lisp ,type (,x ,y) (cl:logior ,x ,y))))
            (define-instance (Monoid ,type)
              (define mempty (lisp ,type () 0)))
            ,@(cl:loop
                 :for member :in members
                 :for member-value :in member-values
                 :append `((declare ,member ,type)
                           (define ,member
                             (lisp ,type () ,member-value)))))))

(cl:defmacro define-pointer (type)
  (cl:let ((ptr (cl:gensym "PTR")))
    `(progn (repr :native cffi:foreign-pointer)
            (define-type ,type)
            (define-instance (Pointer ,type)
              (inline)
              (define (free ,ptr)
                (lisp Unit (,ptr)
                  (cl:prog1 Unit
                    (cffi:foreign-free ,ptr))))))))

(cl:declaim (cl:inline optionalize-pointer))
(cl:defun optionalize-pointer (pointer)
  (cl:declare (cl:type cffi:foreign-pointer pointer)
              (cl:values cl:t cl:&optional))
  (cl:if (cffi:null-pointer-p pointer) None (Some pointer)))

(cl:defmacro define-number-alias (coalton-type foreign-type signedp)
  `(define-type-alias ,coalton-type
     ,(cl:ecase (cffi:foreign-type-size foreign-type)
        ((1) (cl:if signedp 'I8  'U8))
        ((2) (cl:if signedp 'I16 'U16))
        ((4) (cl:if signedp 'I32 'U32))
        ((8) (cl:if signedp 'I64 'U64)))))

(cl:defmacro define-uint-alias (coalton-name foreign-name)
  `(define-number-alias ,coalton-name ,foreign-name cl:nil))

(cl:defmacro define-sint-alias (coalton-name foreign-name)
  `(define-number-alias ,coalton-name ,foreign-name cl:t))

(coalton-toplevel

  (define-class (Pointer :t)
    (free (:t -> Unit)))

  (define-pointer Buffer)
  (define-pointer Callback)

  (define-sint-alias Int :int)
  (define-sint-alias Long :long)
  (define-sint-alias Short :short)

  (define (bitfield-member? x xs) (== (<> xs x) xs)))

(cl:declaim (cl:inline unit-or-errno return-or-errno))

(cl:defun unit-or-errno (ret)
  (cl:if (cl:= -1 ret)
         (Err (zmq-cffi:zmq-errno))
         (Ok Unit)))

(cl:defun return-or-errno (ret)
  (cl:if (cl:= -1 ret)
         (Err (zmq-cffi:zmq-errno))
         (Ok ret)))

