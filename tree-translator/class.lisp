
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
  (set-trans underlying-stat nil)
  (set-active-trans underlying-stat t)
  (let ((obj (make-instance 'dir-entry
                            :stat underlying-stat
                            :parent nil)))
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
  (let* ((return-list nil)
         (real-start (max 0 (- start 2))))
    (when (and (<= start 1) (>= end 1))
      (push (make-dirent ".." 1 :dir) return-list))
    (when (= start 0)
      (push (make-node-dirent "." node) return-list))
    (append return-list
            (mapcar (lambda (inner-entry)
                      (make-node-dirent (name inner-entry) (node inner-entry)))
                    (get-dir-entries node
                                     real-start
                                     (- (1- end) real-start))))))

(define-callback create-directory tree-translator
                 (node user name mode)
  (let ((old (get-entry node name)))
    (cond
      (old
        nil)
      (t
        (add-entry node (make-instance 'dir-entry
                                       :stat (make-stat (stat node) :mode mode)
                                       :parent node)
                   name)))))

(define-callback remove-directory-entry tree-translator
				 (node user name directory-p)
  (remove-dir-entry node name))
