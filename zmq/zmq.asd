;;;; zmq/zmq.asd

(asdf:defsystem "zmq"
  :description ""
  :license "MIT"
  :depends-on ("cffi" "coalton" "trivial-garbage" "zmq-cffi")
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "types-and-constants")
	       (:file "functions")
               (:file "instances")
               (:file "extension")))

