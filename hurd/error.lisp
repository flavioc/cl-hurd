(in-package :cl-hurd.error)

(define-helper-library error)

(defcfun ("get_hurd_error_code" %get-hurd-error-code)
		 :int (id :int))

(define-foreign-type error-type ()
  ()
  (:actual-type :int)
  (:simple-parser err))

;; bits/errno.h
;;
(defconstant +recognized-error-codes+
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
			   (39 :message-too-long)
			   (40 :protocol-wrong-type-socket)
			   (41 :protocol-unavailable)
			   (42 :protocol-not-supported)
			   (43 :socket-type-not-supported)
			   (44 :operation-not-supported)
			   (45 :address-family-not-supported)
			   (46 :adress-already-in-use)
			   (47 :address-unavailable)
			   (48 :network-down)
			   (49 :network-unreachable)
			   (50 :network-dropped-connection)
			   (51 :connection-abort)
			   (52 :connection-reset-by-peer)
			   (53 :no-buffer-space)
			   (54 :transport-endpoint-already-connected)
			   )) ; FIXME: add more

(define-condition unrecognized-error-code (error)
  ((code :initarg :code :reader code)))

(defmacro error-id-to-symbol (var)
	`(case ,var
	   ,@(loop for (id name) in +recognized-error-codes+
			   collect (list (%get-hurd-error-code id) name))
	   (0 t)
	   (otherwise
		 (warn "Identifier ~a not recognized" ,var)
		 nil)))

(defmacro error-symbol-to-id (var)
  `(cond
	 ((eq ,var t) 0)
	 (t
	   (case ,var
		 ,@(loop for (id name) in +recognized-error-codes+
				 collect (list name (%get-hurd-error-code id)))
		 (otherwise 
		   (error 'unrecognized-error-code :code ,var))))))

(defmethod translate-from-foreign (value (type error-type))
  "Translates an error value to a symbol"
  (error-id-to-symbol value))

(defmethod translate-to-foreign (value (type error-type))
  "Translates a lisp error code to a foreign one (ints)"
  (error-symbol-to-id value))
