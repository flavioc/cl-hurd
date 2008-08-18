
(defpackage :null-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator))

(in-package :null-translator)

(defclass null-translator (translator)
  ()
  (:documentation "The null-translator."))

(define-callback make-root-node null-translator
                 (underlying-node underlying-stat)
  (declare (ignore underlying-node))
  (let ((mode (make-mode :perms '((:owner :read :write)
                                  (:group :read :write)
                                  (:others :read :write))
                         :type :chr)))
    (make-instance 'node
                   :stat (make-stat underlying-stat
                                    :mode mode))))

(define-callback read-file null-translator
                 (node user start amount stream)
  (declare (ignore translator node user start amount stream))
  t)

(define-callback write-file null-translator
                 (node user offset stream amount)
  (declare (ignore translator node user offset amount))
  ; Empty the stream to look like we used it all.
  (loop while (read-byte stream nil))
  t)

(defun main ()
  (run-translator (make-instance 'null-translator
                                 :name "null-translator")))

(main)

