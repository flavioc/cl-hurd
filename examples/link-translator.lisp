
(defpackage :link-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator))

(in-package :link-translator)

(defconstant +target-link+ (first ext:*args*))

(defclass link-translator (translator)
  ()
  (:documentation "The link-translator."))

(define-callback make-root-node link-translator
                 (underlying-node underlying-stat)
  (declare (ignore underlying-node))
  (let ((obj (make-instance 'node
                            :stat (make-stat underlying-stat
                                             :type :lnk))))
    (setf (link obj) +target-link+)
    obj))

(define-callback report-access link-translator
                 (node user)
  (when (has-access-p node user :read)
    '(:read)))

(defun main ()
  (run-translator (make-instance 'link-translator
                                 :name "link-translator")))

(main)

