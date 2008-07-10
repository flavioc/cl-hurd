
(in-package :hurd-common)

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
  (let* ((name-string (string-downcase (substitute #\_ #\-
                                                   (string name))))
         (library-name (concatenate 'string
                                    name-string
                                    "_stubs.so"))
         (full-name (intern library-name)))
    `(progn
       (define-foreign-library ,full-name
         (:unix ,library-name))
       (use-foreign-library ,full-name))))

(defmacro with-gensyms ((&rest names) &body body)
  "Use a list of generated symbols"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro unless-return (call &body body)
  "Evaluates 'call' and returns its value if true, else evaluates and returns 'body'."
  (with-gensyms (ret)
    `(let ((,ret ,call))
       (cond
         (,ret ,ret)
         (t
           ,@body)))))

(defun translate-foreign-list (value ls &optional (order 'from))
  "In a list with key/values finds a value using first/second as key, returning second/first from the item found."
  (let ((item (find value ls :key (if (eq order 'from)
                                    #'first
                                    #'second))))
    (when item
      (if (eq order 'from)
        (second item)
        (first item)))))

(defmacro select-error (error-code result)
  "If error-code is success returns result, else returns multiple values 'nil' and 'error-code'."
  `(cond
     ((eq ,error-code t)
      ,result)
     (t
       (values nil ,error-code))))

(defmacro with-cleanup (cleanup &body body)
  "Unwind-protect with multiple expressions."
  `(unwind-protect (progn ,@body)
     ,cleanup))

(defmacro chained-bit-op (op &body ls)
  "Makes possible to have multiple arguments, instead of only two, in a boole operation."
  (if (null ls)
    0
    `(boole ,op
            ,(first ls)
            (chained-bit-op ,op ,@(rest ls)))))

(defun %find-different (str len chr pos)
  "Finds the position of a char different than chr from 'pos' in 'str'"
  (loop for i from pos below len
        for ch = (char str i)
        when (not (eql ch chr))
        return i))

(defun split-path (str)
  "Splits a path into a list with each component. Examples:
a/b/c -> ('a' 'b' 'c')
/a/b -> ('a' 'b')
a///b -> ('a' 'b')
a/b/c/ -> ('a', 'b', 'c', '') pay attention to the last component!"
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
  "Joins a path previously split by split-path."
  (string-left-trim "/" (reduce (lambda (all x)
                                  (concatenate 'string all "/" x))
                                ls
                                :initial-value "")))
