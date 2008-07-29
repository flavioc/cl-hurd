
(in-package :hurd-translator)

(defmacro define-interface (what name params &body body)
  "Defines a new interface callback of type 'what' and name 'name'."
  (with-gensyms (result)
	  `(define-hurd-interface ,what ,name ,params
       ; Remove 'declare' declarative clauses first.
       ,(remove-declare body)
		   ;(warn "enter at ~s ~s~%" (quote ,what) (quote ,name))
		   (let ((,result (when *translator*
                        ,@body)))
			 (if (null ,result)
         :operation-not-supported
         ,result)))))

;; Specialize define-interface for the various stub modules.

(defmacro def-io-interface (name params &body body)
  "IO callbacks."
  `(define-interface io-routine ,name ,params
                     ,@body))

(defmacro def-fs-interface (name params &body body)
  "FS callbacks."
  `(define-interface fs-routine ,name ,params
                     ,@body))

(defmacro def-fsys-interface (name params &body body)
  "FSYS callbacks."
  `(define-interface fsys-routine ,name ,params
					 ,@body))

(defmacro stack-servers (in out &body ls)
  "Generates a call to 'or' with arguments from 'ls'."
  `(or ,@(mapcar (lambda (fun) `(,fun ,in ,out)) ls)))

(defmacro with-lookup (name port &body body)
  "Lookups 'port' on the translator bucket and assigns it to 'name'."
  `(let ((,name (bucket-lookup-port (port-bucket *translator*)
                             ,port)))
     (refresh-node *translator* (get-node ,name) (get-user ,name))
     ,@body))
