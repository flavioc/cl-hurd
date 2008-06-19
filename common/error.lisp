(define-helper-library error)

(defcfun ("get_hurd_error_code" %get-hurd-error-code)
		 :int (id :int))

(define-foreign-type error-type ()
  ()
  (:actual-type :int)
  (:simple-parser err))

;; bits/errno.h
;; these errors codes are generated by _HURD_ERRNO(x).
;;
(defconstant +recognized-standard-codes+
			 '((1 :not-permitted)
			   (2 :no-such-file)
			   (3 :no-such-process)
			   (4 :interrupted-syscall)
			   (5 :io-error)
			   (6 :no-such-device-address)
			   (7 :argument-list-too-long)
			   (8 :exec-format-error)
			   (9 :bad-fd)
			   (10 :no-child-procs)
			   (11 :deadlock-avoided)
			   (12 :no-memory)
			   (13 :permission-denied)
			   (14 :bad-address)
			   (15 :block-device-required)
			   (16 :resource-busy)
			   (17 :file-exists)
			   (18 :invalid-cross-device-link)
			   (19 :no-such-device)
			   (20 :not-directory)
			   (21 :is-a-directory)
			   (22 :invalid-argument)
			   (24 :too-many-open-files)
			   (23 :too-many-open-files-system)
			   (25 :inappropriate-ioctl-device)
			   (26 :text-file-busy)
			   (27 :file-too-large)
			   (28 :no-space-left)
			   (29 :illegal-seek)
			   (30 :read-only-fs)
			   (31 :too-many-links)
			   (32 :broken-pipe)
			   (33 :num-argument-out-domain)
			   (34 :num-result-out-range)
			   (35 :resource-unavailable)
			   (36 :operation-in-progress)
			   (37 :operation-already-in-progress)
			   (38 :socket-op-on-nonsocket)
			   (40 :message-too-long)
			   (41 :protocol-wrong-type-socket)
			   (42 :protocol-unavailable)
			   (43 :protocol-not-supported)
			   (44 :socket-type-not-supported)
			   (45 :operation-not-supported)
			   (46 :protocol-family-not-supported)
			   (47 :address-family-not-supported)
			   (48 :address-family-in-use)
			   (49 :address-unavailable)
			   (50 :network-down)
			   (51 :network-unreachable)
			   (52 :network-dropped-connection)
			   (53 :connection-abort)
			   (54 :connection-reset-by-peer)
			   (55 :no-buffer-space)
			   (56 :transport-endpoint-already-connected)
			   (57 :transport-endpoint-not-connected)
			   (39 :destination-address-required)
			   (58 :transport-endpoint-shutdown)
			   (59 :too-many-references)
			   (60 :connection-timed-out)
			   (61 :connection-refused)
			   (62 :too-many-symbolic-links)
			   (63 :file-name-too-long)
			   (64 :host-unreachable)
			   (65 :no-route-to-host)
			   (66 :directory-not-empty)
			   (67 :too-many-processes)
			   (68 :too-many-users)
			   (69 :disk-quota-exceeded)
			   (70 :stale-nfs-file-handle)
			   (71 :object-is-remote)
			   (72 :bad-rpc)
			   (73 :rpc-version-mismatch)
			   (74 :rpc-program-unavailable)
			   (75 :rpc-program-version-mismatch)
			   (76 :rpc-bad-procedure)
			   (77 :no-locks-available)
			   (79 :inappropriate-file-type)
			   (80 :authentication-error)
			   (81 :need-authentication)
			   (78 :function-not-implemented)
			   (118 :not-supported)
			   (106 :invalid-wide-character)
			   (100 :inappropriate-operation-background)
			   (101 :translator-died)
			   (102 :ed-error-code-unknown) ; ?
			   (103 :egregious)
			   (104 :hopeless-error) ; computer bought the farm!
			   (105 :gratuitous-error)
			   (107 :bad-message)
			   (108 :identifier-removed)
			   (109 :multihop-attempted)
			   (110 :np-data-available)
			   (111 :no-link)
			   (112 :invalid-message-type)
			   (113 :out-streams-resources)
			   (114 :device-not-stream)
			   (115 :value-too-large)
			   (116 :protocol-error)
			   (117 :timer-expired)
			   (119 :operation-canceled)))

;; mach/kern_return.h
;; errors sent by the kernel.
;;
(defconstant +recognized-kernel-codes+
			 '((1 :invalid-address)
			   (2 :protection-failure)
			   (3 :no-space)
			   (4 :invalid-argument)
			   (5 :failure)
			   (6 :resource-shortage)
			   (7 :not-receiver)
			   (8 :no-access)
			   (9 :memory-failure)
			   (10 :memory-error)
			   (11 :already-in-set)
			   (12 :not-in-set)
			   (13 :name-exists)
			   (14 :aborted)
			   (15 :invalid-name)
			   (16 :invalid-task)
			   (17 :invalid-right)
			   (18 :invalid-value)
			   (19 :user-refs-overflow)
			   (20 :invalid-capability)
			   (21 :right-exists)
			   (22 :invalid-host)
			   (23 :memory-present)
			   (24 :write-protection-failure)
			   (26 :terminated)))

;; from mach/mig_errors.h
;; errors that are produced by MIG.
;;
(defconstant +recognized-mig-codes+
			 '((-300 :type-error)
			   (-301 :reply-mismatch)
			   (-302 :remote-error)
			   (-303 :bad-id)
			   (-304 :bad-arguments)
			   (-305 :no-reply)
			   (-306 :exception)
			   (-307 :array-too-large)
			   (-308 :server-died)
			   (-309 :destroy-request)))

;; from mach/message.h
;; errors generated by the mach messaging mechanism.
;; these errors are represented in hexadecimal
;;
(defconstant +recognized-message-codes+
			 '((#x10000001 :send-in-progress)
			   (#x10000002 :send-invalid-data)
			   (#x10000003 :send-invalid-dest)
			   (#x10000004 :send-timed-out)
			   (#x10000005 :send-will-notify)
			   (#x10000006 :send-notify-in-progress)
			   (#x10000007 :send-interrupted)
			   (#x10000008 :send-msg-too-small)
			   (#x10000009 :send-invalid-reply)
			   (#x1000000a :send-invalid-right)
			   (#x1000000b :send-invalid-notify)
			   (#x1000000c :send-invalid-memory)
			   (#x1000000d :send-no-buffer)
			   (#x1000000e :send-no-notify)
			   (#x1000000f :send-invalid-type)
			   (#x10000010 :send-invalid-header)
			   (#x10004001 :rcv-in-progress)
			   (#x10004002 :rcv-invalid-name)
			   (#x10004003 :rcv-timed-out)
			   (#x10004004 :rcv-too-large)
			   (#x10004005 :rcv-interrupted)
			   (#x10004006 :rcv-port-changed)
			   (#x10004007 :rcv-invalid-notify)
			   (#x10004008 :rcv-invalid-data)
			   (#x10004009 :rcv-port-died)
			   (#x1000400a :rcv-in-set)
			   (#x1000400b :rcv-header-error)
			   (#x1000400c :rcv-body-error)))

;; from device/device_types.h
;; errors generated by device drivers.
;;
(defconstant +recognized-device-codes+
			 '((2500 :hardware-io-error)
			   (2501 :hardware-would-block)
			   (2502 :hardware-no-such-device)
			   (2503 :hardware-exclusive-opened)
			   (2504 :hardware-device-shutdown)
			   (2505 :hardware-invalid-operation)
			   (2506 :hardware-invalid-record-num)
			   (2507 :hardware-invalid-io-size)
			   (2508 :hardware-no-memory)
			   (2509 :hardware-device-readonly)))

;; group all the above error codes
(defconstant +recognized-error-codes+
	     (append +recognized-kernel-codes+
		     +recognized-mig-codes+
		     +recognized-device-codes+
		     +recognized-message-codes+
		     (mapcar (lambda (item)
			       (list (%get-hurd-error-code (first item))
				     (second item)))
			       +recognized-standard-codes+)))

(define-condition unrecognized-error-code (error)
  ((code :initarg :code :reader code)))

(defmethod translate-from-foreign (value (type (eql 'err)))
  "Translates an error value to a symbol"
  (cond
    ((zerop value) t) ; success!
    (t
      (let ((item (find value +recognized-error-codes+ :key #'first)))
	(if item
	  (second item)
	  (warn "Identifier ~a not recognized" value))))))

(defmethod translate-to-foreign (value (type (eql 'err)))
  "Translates a lisp error code to a foreign one (ints)"
  (cond
    ((eq value t) 0) ; success!
    (t
      (let ((item (find value +recognized-error-codes+ :key #'second)))
	(if item
	  (first item)
	  (error 'unrecognized-error-code :code value))))))
