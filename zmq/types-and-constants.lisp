;;;; zmq/types-and-constants.lisp

(named-readtables:in-readtable coalton:coalton)

(in-package #:zmq)

(coalton-toplevel

  (define-uint-alias SizeT   zmq-cffi:size-t)
  (define-uint-alias UInt8T  zmq-cffi:uint8-t)
  #+(or WIN32 WIN64) (define-uint-alias ZMQFDT zmq-cffi:zmq-fd-t)
  #-(or WIN32 WIN64) (define-sint-alias ZMQFDT zmq-cffi:zmq-fd-t)
  (define-uint-alias UInt32T zmq-cffi:uint64-t)
  (define-uint-alias UInt64T zmq-cffi:uint64-t)
  #-WIN32 (progn (repr :native cl:t) (define-type SigsetT))

  (define-struct Version
    (major UFix)
    (minor UFix)
    (patch UFix))

  (define-instance (Into Version String)
    (define (into (Version major minor patch))
      (lisp String (major minor patch)
        (cl:format cl:nil "~D.~D.~D" major minor patch))))

  (declare +zmq-version+ Version)
  (define +zmq-version+
    (Version
     (lisp UFix () zmq-cffi:zmq-version-major)
     (lisp UFix () zmq-cffi:zmq-version-minor)
     (lisp UFix () zmq-cffi:zmq-version-patch)))

  (define-pointer Context)
  (define-pointer Socket)
  (define-pointer Message)

  (declare new-message (Unit -> Message))
  (define (new-message)
    (lisp Message ()
      (cffi:foreign-alloc '(:struct zmq-cffi:zmq-msg-t))))

  (define-pointer Pollitem)

  (declare new-pollitem (Unit -> Pollitem))
  (define (new-pollitem)
    (lisp Pollitem ()
      (cffi:foreign-alloc '(:struct zmq-cffi:zmq-pollitem-t))))

  (declare new-pollitem-array (UFix -> Tuple Pollitem (List Pollitem)))
  (define (new-pollitem-array nitems)
    (lisp (Tuple Pollitem (List Pollitem)) (nitems)
      (cl:loop :with type := '(:struct zmq-cffi:zmq-pollitem-t)
               :with ptr  := (cffi:foreign-alloc type :count nitems)
               :with step := (cffi:foreign-type-size type)
               :for index :below nitems
               :collect (cffi:inc-pointer ptr (cl:* index step)) :into items
               :finally (cl:return
                          (call-coalton-function Tuple ptr items)))))

  (declare set-pollitem-slots
           (Pollitem -> Socket -> ZMQFDT -> PollOption -> PollOption -> Unit))
  (define (set-pollitem-slots item socket fd events revents)
    (lisp Unit (item socket fd events revents)
      (cl:let ((type '(:struct zmq-cffi:zmq-pollitem-t)))
        (cl:setf (cffi:foreign-slot-value item type 'zmq-cffi:socket)  socket
                 (cffi:foreign-slot-value item type 'zmq-cffi:fd)      fd
                 (cffi:foreign-slot-value item type 'zmq-cffi:events)  events
                 (cffi:foreign-slot-value item type 'zmq-cffi:revents) revents)
        Unit)))

  (declare pollitem-socket (Pollitem -> Socket))
  (define (pollitem-socket item)
    (lisp Socket (item)
      (cffi:foreign-slot-value
       item
       '(:struct zmq-cffi:zmq-pollitem-t)
       'zmq-cffi:socket)))

  (declare pollitem-fd (Pollitem -> ZMQFDT))
  (define (pollitem-fd item)
    (lisp ZMQFDT (item)
      (cffi:foreign-slot-value
       item
       '(:struct zmq-cffi:zmq-pollitem-t)
       'zmq-cffi:fd)))

  (declare pollitem-events (Pollitem -> PollOption))
  (define (pollitem-events item)
    (lisp PollOption (item)
      (cffi:foreign-slot-value
       item
       '(:struct zmq-cffi:zmq-pollitem-t)
       'zmq-cffi:events)))

  (declare pollitem-revents (Pollitem -> PollOption))
  (define (pollitem-revents item)
    (lisp PollOption (item)
      (cffi:foreign-slot-value
       item
       '(:struct zmq-cffi:zmq-pollitem-t)
       'zmq-cffi:revents)))

  (define-zmq-open-union ZMQError
    enotsup
    eprotonosupport
    enobufs
    enetdown
    eaddrinuse
    eaddrnotavail
    econnrefused
    einprogress
    enotsock
    emsgsize
    eafnosupport
    enetunreach
    econnaborted
    econnreset
    enotconn
    etimedout
    ehostunreach
    enetreset
    efsm
    enocompatproto
    eterm
    emthread)

  (define-zmq-closed-union ContextOption
    zmq-io-threads 
    zmq-max-sockets 
    zmq-socket-limit 
    zmq-thread-priority 
    zmq-thread-sched-policy 
    zmq-max-msgsz 
    zmq-msg-t-size 
    zmq-thread-affinity-cpu-add 
    zmq-thread-affinity-cpu-remove 
    zmq-thread-name-prefix 
    zmq-io-threads-dflt 
    zmq-max-sockets_dflt 
    zmq-thread-priority-dflt 
    zmq-thread-sched-policy-dflt)

  (define-zmq-closed-union SocketType
    zmq-pair
    zmq-pub
    zmq-sub
    zmq-req
    zmq-rep
    zmq-dealer
    zmq-router
    zmq-pull
    zmq-push
    zmq-xpub
    zmq-xsub)

  (define-zmq-closed-union SocketOption
    zmq-affinity 
    zmq-routing-id 
    zmq-subscribe 
    zmq-unsubscribe 
    zmq-rate 
    zmq-recovery-ivl 
    zmq-sndbuf 
    zmq-rcvbuf 
    zmq-rcvmore 
    zmq-fd 
    zmq-events 
    zmq-type 
    zmq-linger 
    zmq-reconnect-ivl 
    zmq-backlog 
    zmq-reconnect-ivl-max 
    zmq-maxmsgsize 
    zmq-sndhwm 
    zmq-rcvhwm 
    zmq-multicast-hops 
    zmq-rcvtimeo 
    zmq-sndtimeo 
    zmq-last-endpoint 
    zmq-router-mandatory 
    zmq-tcp-keepalive 
    zmq-tcp-keepalive-cnt 
    zmq-tcp-keepalive-idle 
    zmq-tcp-keepalive-intvl 
    zmq-immediate 
    zmq-xpub-verbose 
    zmq-router-raw 
    zmq-ipv6 
    zmq-mechanism 
    zmq-plain-server 
    zmq-plain-username 
    zmq-plain-password 
    zmq-curve-server 
    zmq-curve-publickey 
    zmq-curve-secretkey 
    zmq-curve-serverkey 
    zmq-probe-router 
    zmq-req-correlate 
    zmq-req-relaxed 
    zmq-conflate 
    zmq-zap-domain 
    zmq-router-handover 
    zmq-tos 
    zmq-connect-routing-id 
    zmq-gssapi-server 
    zmq-gssapi-principal 
    zmq-gssapi-service-principal 
    zmq-gssapi-plaintext 
    zmq-handshake-ivl 
    zmq-socks-proxy 
    zmq-xpub-nodrop 
    zmq-blocky 
    zmq-xpub-manual 
    zmq-xpub-welcome-msg 
    zmq-stream-notify 
    zmq-invert-matching 
    zmq-heartbeat-ivl 
    zmq-heartbeat-ttl 
    zmq-heartbeat-timeout 
    zmq-xpub-verboser 
    zmq-connect-timeout 
    zmq-tcp-maxrt 
    zmq-thread-safe 
    zmq-multicast-maxtpdu 
    zmq-vmci-buffer-size 
    zmq-vmci-buffer-min-size 
    zmq-vmci-buffer-max-size 
    zmq-vmci-connect-timeout 
    zmq-use-fd 
    zmq-gssapi-principal-nametype 
    zmq-gssapi-service-principal-nametype 
    zmq-bindtodevice)

  (define-zmq-closed-union MessageOption
    zmq-more
    zmq-shared)

  (define-zmq-bitfield SendRecvFlag
    zmq-dontwait
    zmq-sndmore)

  (define-zmq-closed-union SecurityMechanism
    zmq-null
    zmq-plain
    zmq-curve
    zmq-gssapi)

  (define-zmq-closed-union RadioDishProtocol
    zmq-group-max-length)

  (define-zmq-bitfield SocketTransportEvent
    zmq-event-connected
    zmq-event-connect-delayed
    zmq-event-connect-retried
    zmq-event-listening
    zmq-event-bind-failed
    zmq-event-accepted
    zmq-event-accept-failed
    zmq-event-closed
    zmq-event-close-failed
    zmq-event-disconnected
    zmq-event-monitor-stopped
    zmq-event-all
    zmq-event-handshake-failed-no-detail
    zmq-event-handshake-succeeded
    zmq-event-handshake-failed-protocol
    zmq-event-handshake-failed-auth)

  (define-zmq-open-union ProtocolError)

  (define-zmq-bitfield PollOption
    zmq-pollin
    zmq-pollout
    zmq-pollerr
    zmq-pollpri))

