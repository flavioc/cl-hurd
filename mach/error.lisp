
; kern_return_t
(define-foreign-type kern-return-type ()
	()
	(:actual-type :int)
	(:simple-parser kern-return))

;; mach/kern_return.h
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

;; from mach/message.h
(defconstant +recognized-message-codes+
			 '(("10000001" :send-in-progress)
			   ("10000002" :send-invalid-data)
			   ("10000003" :send-invalid-dest)
			   ("10000004" :send-timed-out)
			   ("10000005" :send-will-notify)
			   ("10000006" :send-notify-in-progress)
			   ("10000007" :send-interrupted)
			   ("10000008" :send-msg-too-small)
			   ("10000009" :send-invalid-reply)
			   ("1000000a" :send-invalid-right)
			   ("1000000b" :send-invalid-notify)
			   ("1000000c" :send-invalid-memory)
			   ("1000000d" :send-no-buffer)
			   ("1000000e" :send-no-notify)
			   ("1000000f" :send-invalid-type)
			   ("10000010" :send-invalid-header)
			   ("10004001" :rcv-in-progress)
			   ("10004002" :rcv-invalid-name)
			   ("10004003" :rcv-timed-out)
			   ("10004004" :rcv-too-large)
			   ("10004005" :rcv-interrupted)
			   ("10004006" :rcv-port-changed)
			   ("10004007" :rcv-invalid-notify)
			   ("10004008" :rcv-invalid-data)
			   ("10004009" :rcv-port-died)
			   ("1000400a" :rcv-in-set)
			   ("1000400b" :rcv-header-error)
			   ("1000400c" :rcv-body-error)))

;; from mach/mig_errors.h

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

(define-condition unrecognized-return-code (error)
    ((code :initarg :code :reader code)))

(defmacro return-id-to-symbol (var)
  `(case ,var
	 ,@(loop for (id name) in (append +recognized-kernel-codes+ +recognized-mig-codes+)
			 collect (list id name))
	 ,@(loop for (str name) in +recognized-message-codes+
			 collect (list (string->integer str 16) name))
	 (0 t) ; success
	 (otherwise
	   (warn "Identifier ~a not recognized" ,var)
	   nil)))

(defmacro return-symbol-to-id (var)
  `(cond
	 ((eq ,var t) 0)
	 (t
	   (case ,var
		 ,@(loop for (id name) in (append +recognized-kernel-codes+ +recognized-mig-codes+)
				 collect (list name id))
		 ,@(loop for (str name) in +recognized-message-codes+
				 collect (list name (string->integer str 16)))
		 (otherwise 
		   (error 'unrecognized-return-code :code ,var))))))

(defmethod translate-from-foreign (value (type kern-return-type))
  "Translates an error value to a symbol"
  (return-id-to-symbol value))

(defmethod translate-to-foreign (value (type kern-return-type))
  "Translates a lisp error code to a foreign one (ints)"
  (return-symbol-to-id value))

