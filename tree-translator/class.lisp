
(in-package :hurd-tree-translator)

;;
;; This file a special kind of translator:
;; the tree-translator
;; It exposes without much work the directory structure.
;;

(defclass tree-translator (translator)
  ((name :initform "tree-translator"
         :documentation "Translator name"))
  (:documentation "The tree-translator."))

;; It ensures the root node is a directory
;; and calls the function fill-root-node to fill the directory
;; structure.
(define-callback make-root-node tree-translator
                 (underlying-stat)
  (when (not (is-dir-p underlying-stat))
    (propagate-read-to-execute underlying-stat))
  (set-type underlying-stat 'dir)
  (set-trans underlying-stat nil)
  (let ((obj (make-dir "root" underlying-stat)))
    (fill-root-node translator obj)
    obj))

(defmethod fill-root-node ((translator tree-translator) (root dir-entry))
  "This should be used to construct the directory structure. 'root' is the newly created
root node."
  nil)

(define-callback dir-lookup tree-translator
				 (node user filename)
  (cond
    ((string= filename ".")
     node)
    ((string= filename "..")
     (parent node))
    (t
      (get-entry node filename))))

(define-callback number-of-entries tree-translator
				 (node user)
  (dir-size node))

(define-callback get-entries tree-translator
				(node user start end)
  (let* ((all (loop for value being the hash-values of (entries node)
                    using (hash-key key)
                    collect (make-dirent key value)))
         (sorted (sort all (lambda (a b) (string< (name a) (name b))))))
    (subseq sorted start (1+ end))))

(define-callback create-directory tree-translator
				 (node user name mode)
  (let ((new-stat (make-stat (stat node))))
    (setf (stat-get new-stat 'mode) mode)
    (add-entry node (make-dir name new-stat node))))

(define-callback remove-entry tree-translator
				 (node user name directory-p)
  (remove-dir-entry node name))
