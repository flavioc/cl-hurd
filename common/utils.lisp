
(defun largest-representable-number (bits)
  "Largest representable number having 'bits' bits available"
  (1- (expt 2 bits)))

(defun num-bits (bytes)
  "Tells how many bits there are in a number of bytes"
  (* 8 bytes))

(defmacro define-helper-library (name)
  (let* ((name-string (string-downcase (string name)))
		 (library-name (concatenate 'string name-string ".so"))
		 (full-name (intern library-name)))
	`(progn
	   (define-foreign-library ,full-name
	     (:unix ,library-name))
	   (use-foreign-library ,full-name))))
