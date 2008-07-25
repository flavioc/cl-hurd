
(in-package :hurd-example-translators)

(defclass null-translator (translator)
  ((name :initform "null-translator"
         :documentation "Translator name"))
  (:documentation "The null-translator."))

(define-callback make-root-node null-translator
                 (underlying-stat)
  (let ((mode (make-mode :perms '((owner read write)
                                  (group read write)
                                  (others read write))
                         :type :chr)))
    (make-instance 'node
                   :stat (make-stat underlying-stat
                                    :mode mode))))

(define-callback file-read null-translator
                 (node user start amount stream)
  (declare (ignore translator node user start amount stream))
  t)

(define-callback file-write null-translator
                 (node user offset stream)
  (declare (ignore translator node user offset))
  ; Empty the stream to look like we used it all.
  (loop while (read-byte stream nil))
  t)

(defun main ()
  (run-translator (make-instance 'null-translator)))

(main)
