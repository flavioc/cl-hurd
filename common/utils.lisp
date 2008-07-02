
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
  `(unwind-protect (progn ,@body)
	 ,cleanup))

;; memcpy

(defcfun ("memcpy" %memcpy)
		 :void
		 (dest :pointer)
		 (src :pointer)
		 (size :unsigned-int))

(defun memcpy (dest src size)
  (%memcpy dest src size))

(defmacro chained-bit-op (op &body ls)
  (if (null ls)
	0
	`(boole ,op
			,(first ls)
			(chained-bit-op ,op ,@(rest ls)))))

(defun %find-different (str len chr pos)
  (loop for i from pos below len
		for ch = (char str i)
		when (not (eql ch chr))
		return i))

(defun split-path (str)
  (let ((len (length str)))
	(loop for i = (%find-different str len #\/ 0)
		  then (%find-different str len #\/ (1+ j))
		  as j = (if (null i) nil
				   (position #\/ str :start i))
		  collect (progn
					(cond
					  ((and (null j)
							(null i))
					   "")
					  ((null j)
					   (subseq str i len))
					  (t
						(subseq str i j))))
		  while j)))

(defun join-path (ls)
  (string-left-trim "/" (reduce (lambda (all x)
								  (concatenate 'string all "/" x))
								ls
								:initial-value "")))
