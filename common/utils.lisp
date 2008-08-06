
(in-package :hurd-common)

(defun largest-representable-number (bits)
  "Largest representable number having 'bits' bits available"
  (1- (expt 2 bits)))

(defun num-bits (bytes)
  "Tells how many bits there are in a number of bytes"
  (* 8 bytes))

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

(defun translate-foreign-list (value ls &optional (order :from))
  "In a list with key/values finds a value using first/second as key, returning second/first from the item found."
  (let ((item (find value ls :key (if (eq order :from)
                                    #'first
                                    #'second))))
    (when item
      (if (eq order :from)
        (second item)
        (first item)))))

(defmacro select-error (error-code &optional (result t))
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

(defun %convert-list (item)
  (if (symbolp item)
    (list item)
    item))

(defun flag-is-p (flags flag)
  "Checks if flags has the flag or flag's list 'flag' enabled."
  (let ((new-list (%convert-list flag)))
    (equal new-list
           (intersection flags new-list))))

(defun enable-flags (flags new-flags)
  "Enable all flags in new-flags."
  (union flags (%convert-list new-flags)))

(defun disable-flags (flags old-flags)
  "Disable all flags in old-flags."
  (set-difference flags (%convert-list old-flags)))

(defun only-flags (flags new-flags)
  "Only enable flags in new-flags."
  (intersection flags (%convert-list new-flags)))

(defun free-memory-list (ls)
  "Frees a list with pointers."
  (loop for item in ls
        do (when (and (pointerp item)
                      (not (null-pointer-p item)))
             (foreign-free item))))

(defun foreign-string-zero-separated-to-list (ptr ptr-len)
  "Converts a foreign string sequence separated by '\0' into a list of lisp strings."
  (let ((total-len 0))
    (loop until (eq total-len ptr-len)
          collect (let* ((str (foreign-string-to-lisp ptr))
                         (len (1+ (length str))))
                    (incf-pointer ptr len)
                    (incf total-len len)
                    str))))

(defmacro concatenate-string (&body rest)
  "Use concatenate to concat strings."
  `(concatenate 'string ,@rest))

(defmacro with-stream ((stream-name init) &body body)
  "Open stream with name 'stream-name' and initialization 'init' and the close it."
  `(let ((,stream-name ,init))
     (with-cleanup (close ,stream-name)
       ,@body)))

(defun string-list-len (ls)
  "Given a list of strings, return a list of string lengths plus one."
  (mapcar #'1+ (mapcar #'length ls)))

(defun sum-list (ls)
  "Return sum of an number list."
  (reduce #'+ ls))

(defun list-to-foreign-string-zero-separated (ls ptr &optional ls-len)
  "Write a list of strings into a foreign array. Strings are '\0'-separated.
If you have the list with the length for each string pass it in ls-len."
  (unless ls-len
    (setf ls-len (string-list-len ls)))
  (loop for item in ls
        for item-len in ls-len
        do (progn
             (lisp-string-to-foreign item
                                     ptr
                                     item-len)
             (incf-pointer ptr item-len))))

(defmacro remove-declare (body)
  "Removes a potencial declare directive from body and returns it."
  `(when (and (>= (length ,body) 1)
              (eq (first (first ,body)) 'declare))
     (let ((ret (first ,body)))
       (setf ,body (rest ,body))
       ret)))

(defun microsecs->nanosecs (microsecs)
  (* microsecs 1000))

(defun nanosecs->microsecs (nanosecs)
  (/ nanosecs 1000))
