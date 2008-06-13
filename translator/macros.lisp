
;; macro that defines a new interface callback
;; and then sets it on the routines table
;;
(defmacro define-interface (what name params &body body)
  (let ((callback-fun-name (intern
							(concatenate 'string "%lisp-"
										 (symbol-name name))))
		(keyword-name (intern (string name) "KEYWORD")))
	`(progn
	   (defcallback ,callback-fun-name err ,params
					(when *translator*
					  ,@body))
	   (setf (,what ,keyword-name) (callback ,callback-fun-name)))))

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
