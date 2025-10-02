;;;; zmq-examples/zmq-examples.asd

(asdf:defsystem "zmq-examples"
  :description ""
  :depends-on ("coalton" "cffi" "with-user-abort" "zmq")
  :serial t
  :components ((:file "hwserver")
               (:file "hwclient")
               (:file "version")
               (:file "zhelpers")
               (:file "wuserver")
               (:file "wuclient")
               (:file "rrclient")
               (:file "rrworker")
               (:file "rrbroker")))

(asdf:defsystem "zmq-examples/hwserver"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/hwserver"
  :entry-point "hwserver:cl-main")

(asdf:defsystem "zmq-examples/hwclient"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/hwclient"
  :entry-point "hwclient:cl-main")

(asdf:defsystem "zmq-examples/version"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/version"
  :entry-point "version:cl-main")

(asdf:defsystem "zmq-examples/wuserver"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/wuserver"
  :entry-point "wuserver:cl-main")

(asdf:defsystem "zmq-examples/wuclient"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/wuclient"
  :entry-point "wuclient:cl-main")

(asdf:defsystem "zmq-examples/rrclient"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/rrclient"
  :entry-point "rrclient:cl-main")

(asdf:defsystem "zmq-examples/rrworker"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/rrworker"
  :entry-point "rrworker:cl-main")

(asdf:defsystem "zmq-examples/rrbroker"
  :description ""
  :depends-on ("zmq-examples")
  :build-operation "program-op"
  :build-pathname "bin/rrbroker"
  :entry-point "rrbroker:cl-main")

