;;;; zmq-cffi/cffi.lisp

(in-package #:zmq-cffi)

(cffi:define-foreign-library libzmq
  (:linux   (:or "libzmq" "libzmq.so"))
  (:darwin  (:or "libzmq" "libzmq.dylib"))
  (:windows (:or "libzmq" "libzmq.dll")))

(cffi:defcfun memcpy (:pointer :void)
  (dest (:pointer :void))
  (src  (:pointer :void))
  (n    size-t))

(cffi:defcfun zmq-errno :int)

(cffi:defcfun zmq-strerror :string
  (errnum_ :int))

(cffi:defcfun zmq-version :void
  (major_ (:pointer :int))
  (minor_ (:pointer :int))
  (patch_ (:pointer :int)))

;; Infrastructure.

(cffi:defcfun zmq-ctx-new (:pointer :void))

(cffi:defcfun zmq-ctx-term :int
  (context_ (:pointer :void)))

(cffi:defcfun zmq-ctx-shutdown :int
  (context_ (:pointer :void)))

(cffi:defcfun zmq-ctx-set :int
  (context_ (:pointer :void))
  (option_  :int)
  (optval_  :int))

(cffi:defcfun zmq-ctx-get :int
  (context_ (:pointer :void))
  (option_  :int))

(cffi:defcfun zmq-init (:pointer :void)
  (io-threads_ :int))

(cffi:defcfun zmq-term :int
  (context_ (:pointer :void)))

(cffi:defcfun zmq-ctx-destroy :int
  (context_ (:pointer :void)))

;; Message definition.

(cffi:defctype zmq-free-fn :void)

(cffi:defcfun zmq-msg-init :int
  (msg_ (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-init-size :int
  (msg_  (:pointer (:struct zmq-msg-t)))
  (size_ size-t))

(cffi:defcfun zmq-msg-init-data :int
  (msg_  (:pointer (:struct zmq-msg-t)))
  (data_ (:pointer :void))
  (size_ size-t)
  (ffn_  (:pointer zmq-free-fn))
  (hint_ (:pointer :void)))

(cffi:defcfun zmq-msg-send :int
  (msg_   (:pointer (:struct zmq-msg-t)))
  (s_     (:pointer :void))
  (flags_ :int))

(cffi:defcfun zmq-msg-recv :int
  (msg_   (:pointer (:struct zmq-msg-t)))
  (s_     (:pointer :void))
  (flags_ :int))

(cffi:defcfun zmq-msg-close :int
  (msg_ (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-move :int
  (dest_ (:pointer (:struct zmq-msg-t)))
  (src_  (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-copy :int
  (dest_ (:pointer (:struct zmq-msg-t)))
  (src_  (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-data (:pointer :void)
  (msg_ (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-size size-t
  (msg_ (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-more :int
  (msg_ (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-get :int
  (msg_      (:pointer (:struct zmq-msg-t)))
  (property_ :int))

(cffi:defcfun zmq-msg-set :int
  (msg_      (:pointer (:struct zmq-msg-t)))
  (property_ :int)
  (optval_   :int))

(cffi:defcfun zmq-msg-gets (:pointer :char)
  (msg_     (:pointer (:struct zmq-msg-t)))
  (property (:pointer :char)))

;; Sockets.

(cffi:defcfun zmq-socket (:pointer :void)
  (ctx_  (:pointer :void))
  (type_ :int))

(cffi:defcfun zmq-close :int
  (s_ (:pointer :void)))

(cffi:defcfun zmq-setsockopt :int
  (s_         (:pointer :void))
  (option_    :int)
  (optval     (:pointer :void))
  (optvallen_ size-t))

(cffi:defcfun zmq-getsockopt :int
  (s_         (:pointer :void))
  (option_    :int)
  (optval     (:pointer :void))
  (optvallen_ size-t))

(cffi:defcfun zmq-bind :int
  (s_    (:pointer :void))
  (addr_ :string))

(cffi:defcfun zmq-connect :int
  (s_    (:pointer :void))
  (addr_ :string))

(cffi:defcfun zmq-unbind :int
  (s_    (:pointer :void))
  (addr_ :string))

(cffi:defcfun zmq-disconnect :int
  (s_    (:pointer :void))
  (addr_ :string))

(cffi:defcfun zmq-send :int
  (s_     (:pointer :void))
  (buf_   (:pointer :void))
  (len_   size-t)
  (flags_ :int))

(cffi:defcfun zmq-send-const :int
  (s_     (:pointer :void))
  (buf_   (:pointer :void))
  (len_   size-t)
  (flags_ :int))

(cffi:defcfun zmq-recv :int
  (s_     (:pointer :void))
  (buf_   (:pointer :void))
  (len_   size-t)
  (flags_ :int))

(cffi:defcfun zmq-socket-monitor :int
  (s_      (:pointer :void))
  (addr_   :string)
  (events_ :int))

(cffi:defcfun zmq-poll :int
  (items_   (:pointer (:struct zmq-pollitem-t)))
  (nitems_  :int)
  (timeout_ :long))

;; Proxy.

(cffi:defcfun zmq-proxy :int
  (frontend_ (:pointer :void))
  (backend_  (:pointer :void))
  (capture_  (:pointer :void)))

(cffi:defcfun zmq-proxy-steerable :int
  (frontend_ (:pointer :void))
  (backend_  (:pointer :void))
  (capture_  (:pointer :void))
  (control_  (:pointer :void)))

;; Library capabilities.

(cffi:defcfun zmq-has :int
  (capability_ (:pointer :char)))

(cffi:defcfun zmq-device :int
  (type_     :int)
  (frontend_ (:pointer :void))
  (backend_  (:pointer :void)))

(cffi:defcfun zmq-sendmsg :int
  (s_     (:pointer :void))
  (msg_   (:pointer (:struct zmq-msg-t)))
  (flags_ :int))

(cffi:defcfun zmq-recvmsg :int
  (s_     (:pointer :void))
  (msg_   (:pointer (:struct zmq-msg-t)))
  (flags_ :int))

(cffi:defcstruct iovec)

(cffi:defcfun zmq-sendiov :int
  (s_     (:pointer :void))
  (iov_   (:pointer (:struct iovec)))
  (count_ size-t)
  (flags_ :int))

(cffi:defcfun zmq-recviov :int
  (s_     (:pointer :void))
  (iov_   (:pointer (:struct iovec)))
  (count_ (:pointer size-t))
  (flags_ :int))

;; Encryption.

(cffi:defcfun zmq-z85-encode (:pointer :char)
  (dest_ (:pointer :char))
  (data_ (:pointer uint8-t))
  (size_ size-t))

(cffi:defcfun zmq-z85-decode (:pointer uint8-t)
  (dest_   (:pointer uint8-t))
  (string_ (:pointer :char)))

(cffi:defcfun zmq-curve-keypair :int
  (z85-public-key_ (:pointer :char))
  (z85-secret-key_ (:pointer :char)))

(cffi:defcfun zmq-curve-public :int
  (z85-public-key (:pointer :char))
  (z85-secret-key (:pointer :char)))

;; Atomic utility methods.

(cffi:defcfun zmq-atomic-counter-new (:pointer :void))

(cffi:defcfun zmq-atomic-counter-set :void
  (counter_ (:pointer :void))
  (value_   :int))

(cffi:defcfun zmq-atomic-counter-inc :int
  (counter_ (:pointer :void)))

(cffi:defcfun zmq-atomic-counter-dec :int
  (counter_ (:pointer :void)))

(cffi:defcfun zmq-atomic-counter-value :int
  (counter_ (:pointer :void)))

(cffi:defcfun zmq-atomic-counter-destroy :void
  (counter_p_ (:pointer (:pointer :void))))

;; Timers.

(cffi:defctype zmq-timer-fn :void)

(cffi:defcfun zmq-timers-new (:pointer :void))

(cffi:defcfun zmq-timers-destroy :int
  (timers_p (:pointer (:pointer :void))))

(cffi:defcfun zmq-timers-add :int
  (timers   (:pointer :void))
  (interval size-t)
  (handler  (:pointer zmq-timer-fn))
  (arg      (:pointer :void)))

(cffi:defcfun zmq-timers-cancel :int
  (timers   (:pointer :void))
  (timer_id :int))

(cffi:defcfun zmq-timers-set-interval :int
  (timers   (:pointer :void))
  (timer-id :int)
  (interval size-t))

(cffi:defcfun zmq-timers-reset :int
  (timers   (:pointer :void))
  (timer-id :int))

(cffi:defcfun zmq-timers-timeout :long
  (timers (:pointer :void)))

(cffi:defcfun zmq-timers-execute :int
  (timers (:pointer :void)))

;; Undocumented.

(cffi:defcfun zmq-stopwatch-start (:pointer :void))

(cffi:defcfun zmq-stopwatch-intermediate :unsigned-long
  (watch_ (:pointer :void)))

(cffi:defcfun zmq-stopwatch-stop :unsigned-long
  (watch_ (:pointer :void)))

(cffi:defcfun zmq-sleep :void
  (seconds_ :int))

(cffi:defctype zmq-thread-fn :void)

(cffi:defcfun zmq-threadstart (:pointer :void)
  (func_ (:pointer zmq-thread-fn))
  (arg_  (:pointer :void)))

(cffi:defcfun zmq-threadclose :void
  (thread_ (:pointer :void)))

;; Draft.

(cffi:defcfun zmq-ctx-set-ext :int
  (context_   (:pointer :void))
  (option_    :int)
  (optval_    (:pointer :void))
  (optvallen_ size-t))

(cffi:defcfun zmq-ctx-get-ext :int
  (context_   (:pointer :void))
  (option_    :int)
  (optval_    (:pointer :void))
  (optvallen_ size-t))

(cffi:defcfun zmq-join :int
  (s     (:pointer :void))
  (group (:pointer :char)))

(cffi:defcfun zmq-leave :int
  (s     (:pointer :void))
  (group (:pointer :char)))

(cffi:defcfun zmq-connect-peer uint32-t
  (s_    (:pointer :void))
  (addr_ (:pointer :char)))

(cffi:defcfun zmq-msg-set-routing-id :int
  (msg        (:pointer (:struct zmq-msg-t)))
  (routing-id uint32-t))

(cffi:defcfun zmq-msg-routing-id uint32-t
  (msg (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-set-group :int
  (msg   (:pointer (:struct zmq-msg-t)))
  (group (:pointer :char)))

(cffi:defcfun zmq-msg-group (:pointer :char)
  (msg (:pointer (:struct zmq-msg-t))))

(cffi:defcfun zmq-msg-init-buffer :int
  (msg_  (:pointer (:struct zmq-msg-t)))
  (buf_  (:pointer :void))
  (size_ size-t))

(cffi:defcfun zmq-poller-new (:pointer :void))

(cffi:defcfun zmq-poller-destroy :int
  (poller-p (:pointer (:pointer :void))))

(cffi:defcfun zmq-poller-size :int
  (poller (:pointer :void)))

(cffi:defcfun zmq-poller-add :int
  (poller    (:pointer :void))
  (socket    (:pointer :void))
  (user-data (:pointer :void))
  (events    :short))

(cffi:defcfun zmq-poller-modify :int
  (poller (:pointer :void))
  (socket (:pointer :void))
  (events :short))

(cffi:defcfun zmq-poller-remove :int
  (poller (:pointer :void))
  (socket (:pointer :void)))

(cffi:defcfun zmq-poller-wait :int
  (poller  (:pointer :void))
  (events  (:pointer (:struct zmq-poller-event-t)))
  (timeout :long))

(cffi:defcfun zmq-poller-wait-all :int
  (poller   (:pointer :void))
  (events   (:pointer (:struct zmq-poller-event-t)))
  (n-events :int)
  (timeout  :long))

(cffi:defcfun zmq-poller-fd :int
  (poller (:pointer :void))
  (fd     (:pointer zmq-fd-t)))

(cffi:defcfun zmq-poller-add-fd :int
  (poller    (:pointer :void))
  (fd        zmq-fd-t)
  (user-data (:pointer :void))
  (timeout   :long))

(cffi:defcfun zmq-poller-modify-fd :int
  (poller (:pointer :void))
  (fd     zmq-fd-t)
  (events :short))

(cffi:defcfun zmq-poller-remove-fd :int
  (poller (:pointer :void))
  (fd     zmq-fd-t))

(cffi:defcfun zmq-socket-get-peer-state :int
  (socket          (:pointer :void))
  (routing-id      (:pointer :void))
  (routing-id-size size-t))

(cffi:defcfun zmq-socket-monitor-versioned :int
  (s_             (:pointer :void))
  (addr_          (:pointer :char))
  (events_        uint64-t)
  (event_version_ :int)
  (type_          :int))

(cffi:defcfun zmq-socket-monitor-pipes-stats :int
  (s_ (:pointer :void)))

(cffi:defcfun zmq-ppoll :int
  (items_ (:pointer (:struct zmq-pollitem-t)))
  (nitems_ :int)
  (timeout_ :long)
  (sigmask_ (:pointer #-WIN32 (:struct sigset-t) #+WIN32 :void)))

