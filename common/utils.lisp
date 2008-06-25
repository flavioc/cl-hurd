
(defun largest-representable-number (bits)
  "Largest representable number having 'bits' bits available"
  (1- (expt 2 bits)))

(defun num-bits (bytes)
  "Tells how many bits there are in a number of bytes"
  (* 8 bytes))

(defmacro define-helper-library (name)
  "Defines a new helper library using CFFI"
  (let* ((name-string (string-downcase (string name)))
		 (library-name (concatenate 'string name-string ".so"))
		 (full-name (intern library-name)))
	`(progn
	   (define-foreign-library ,full-name
	     (:unix ,library-name))
	   (use-foreign-library ,full-name))))

(defmacro define-stub-library (name)
  "Defines a new stub library using CFFI"
  (let* ((name-string (string-downcase (string name)))
		 (library-name (concatenate 'string name-string "_stubs.so"))
		 (full-name (intern library-name)))
	`(progn
	   (define-foreign-library ,full-name
	     (:unix ,library-name))
	   (use-foreign-library ,full-name))))

(defmacro with-gensyms ((&rest names) &body body)
  "Use a list of generated symbols"
  `(let ,(loop for n in names collect `(,n (gensym)))
	 ,@body))

(defun char->value (ch)
  (case ch
	(#\0 0)
	(#\1 1)
	(#\2 2)
	(#\3 3)
	(#\4 4)
	(#\5 5)
	(#\6 6)
	(#\7 7)
	(#\8 8)
	(#\9 9)
	(#\a 10)
	(#\b 11)
	(#\c 12)
	(#\d 13)
	(#\e 14)
	(#\f 15)))

(defun string->integer (str base)
  (let ((e -1))
	(reduce (lambda (current accum)
			  (incf e)
			  (+ accum (* (char->value current) (expt base e))))
			str :from-end t :initial-value 0)))

(defmacro unless-return (call &body body)
  (with-gensyms (ret)
    `(let ((,ret ,call))
       (cond
		 (,ret ,ret)
		 (t
		   ,@body)))))

(defun translate-foreign-list (value ls &optional (order 'from))
  (let ((item (find value ls :key (if (eq order 'from)
				    #'first
				    #'second))))
    (when item
      (if (eq order 'from)
	(second item)
	(first item)))))

(defmacro select-error (error-code result)
  `(cond
     ((eq ,error-code t)
      ,result)
     (t
       (values nil ,error-code))))

(defmacro with-cleanup (cleanup &body body)
  `(let ((result (progn ,@body)))
     ,cleanup
     result))
