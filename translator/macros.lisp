
;; macro that defines a new interface callback
;; and then sets it on the routines table
;;
(defmacro define-interface (what name params &body body)
  (with-gensyms (result)
	  `(define-hurd-interface ,what ,name ,params
		  ;(warn "enter at ~s ~s~%" (quote ,what) (quote ,name))
		  (let ((,result (when *translator*
						   ,@body)))
			(if (null ,result)
			  :operation-not-supported
			  ,result)))))

;; specialize define-interface for the various stub modules

;; io
(defmacro def-io-interface (name params &body body)
  `(define-interface io-routine ,name ,params
					 ,@body))

;; fs
(defmacro def-fs-interface (name params &body body)
  `(define-interface fs-routine ,name ,params
					 ,@body))

;; fsys
(defmacro def-fsys-interface (name params &body body)
  `(define-interface fsys-routine ,name ,params
					 ,@body))

;; stacking servers for a demuxer

(defmacro stack-servers (in out &body ls)
  `(or ,@(mapcar (lambda (fun) `(,fun ,in ,out)) ls)))

;; lookup port on a rpc

(defmacro with-lookup (name port &body body)
  `(let ((,name (lookup-port (port-bucket *translator*)
							 ,port)))
	 ,@body))
