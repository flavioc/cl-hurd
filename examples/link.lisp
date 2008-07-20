
(in-package :hurd-example-translators)

(defconstant +target-link+ "../cl-hurd")

(defclass link-translator (translator)
  ((name :initform "link-translator"
         :documentation "Translator name"))
  (:documentation "The link-translator."))

(define-callback make-root-node link-translator
                 (underlying-stat)
  (set-trans underlying-stat nil)
  (set-active-trans underlying-stat t)
  (let ((obj (make-instance 'node :stat underlying-stat)))
    (setf (link obj) +target-link+)
    (warn "stat ~s" (stat obj))
    obj))

(define-callback allow-open-p link-translator (node user flags is-new-p) t)

(define-callback dir-lookup link-translator
				 (node user filename)
  nil)

(define-callback number-of-entries link-translator
				 (node user)
  0)

(define-callback get-entries link-translator
				(node user start end)
  nil)

(define-callback create-directory link-translator
                 (node user name mode)
  nil)

(define-callback remove-directory-entry link-translator
				 (node user name directory-p)
  nil)

(defvar *link-translator*
  (make-instance 'link-translator))

(run-translator *link-translator*)
