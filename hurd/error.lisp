(in-package :cl-hurd.error)

(define-helper-library error)

(defcfun ("get_hurd_error_code" %get-hurd-error-code)
		 :int (id :int))

(define-foreign-type error-type ()
  ()
  (:actual-type :int)
  (:simple-parser err))

(defconstant +recognized-error-codes+
			 '((perm 1)
			   (noent 2)
			   (srch 3))) ; FIXME: add more

(defmacro translate-hurd-error-codes (var &key (direction :id-to-symbol))
  `(case ,var
	 ,@(if (eq direction :id-to-symbol)
		 (list 0 t)
		 (list t 0))
	 ,@(loop for (name id) in +recognized-error-codes+
			 collect (if (eq direction 'id-to-symbol)
					   (list name (%get-hurd-error-code id))
					   (list (%get-hurd-error-code id) name)))
	 (t nil)))

(defmethod translate-from-foreign (value (type error-type))
  "Translates an error value to a symbol"
  (get-hurd-error-codes value :direction :id-to-symbol))

(defmethod translate-to-foreign (value (type error-type))
  "Translates a lisp error code to a foreign one (ints)"
  (translate-hurd-error-codes value :direction :symbol-to-id))
