;;;; zmq-cffi/zmq-cffi.asd

(asdf:defsystem "zmq-cffi"
  :description ""
  :license "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi")
  :serial t
  :components ((:file "package")
	       (:cffi-grovel-file "grovel")
	       (:file "cffi")))

