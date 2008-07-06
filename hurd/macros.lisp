
(in-package :hurd)

(defmacro define-hurd-interface (what name params &body body)
  "Defines a new interface callback and then sets it on the routines table."
  (let ((callback-fun-name (intern
                             (concatenate 'string "%lisp-"
                                          (symbol-name name))))
        (keyword-name (intern (string name) "KEYWORD")))
    `(progn
       (defcallback ,callback-fun-name err ,params
         ,@body)
       (setf (,what ,keyword-name) (callback ,callback-fun-name)))))

