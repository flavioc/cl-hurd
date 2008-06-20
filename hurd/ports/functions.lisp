(defcfun ("ports_create_class" %ports-create-class)
	 ports-class-t
	 (clean-routine :pointer)
	 (dropweak-routine :pointer))

(defmacro ports-create-class (&optional (clean nil)
					(dropweak nil))
  (with-gensyms (clean-callback-name
		  dropweak-callback-name)
    `(progn
       ,(if (not (null clean))
	  `(defcallback ,clean-callback-name :void ((port :pointer))
			(funcall clean port)))
       ,(if (not (null dropweak))
	  `(defcallback ,dropweak-callback-name :void ((port :pointer))
			(funcall dropweak port)))
       (%ports-create-class ,(if (null clean)
			       `(null-pointer)
			       `(callback ,clean-callback-name))
			    ,(if (null dropweak)
			       `(null-pointer)
			       `(callback ,dropweak-callback-name))))))

(defcfun ("ports_create_bucket" %ports-create-bucket)
		 ports-bucket-t)

(defun ports-create-bucket ()
  (%ports-create-bucket))

(defvar *class* (ports-create-class))
(defvar *bucket* (ports-create-bucket))

(define-helper-library create-port)

(defcfun ("create_port" %create-port)
		 port-info-t
		 (port-bucket ports-bucket-t)
		 (port-class ports-class-t)
		 (size :unsigned-int)
		 (error-code :pointer))

(defmethod create-port ((bucket ports-bucket) port-class)
  (with-foreign-pointer (error-code (foreign-type-size :pointer))
   	(let* ((new-port (%create-port
						bucket
						port-class
						(foreign-type-size 'port-info)
						error-code))
		   (translated-error (mem-ref error-code 'err)))
	  (select-error translated-error new-port))))

(defvar *port* (create-port *bucket* *class*))
