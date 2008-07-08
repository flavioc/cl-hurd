
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
  (set-type underlying-stat :dir)
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
  (let* ((return-list nil)
         (real-start (max 0 (- start 2))))
    (when (= start 0)
      (push (make-node-dirent "." node) return-list))
    (when (and (<= start 1) (>= end 1))
      (push (make-dirent ".." 1 :dir) return-list))
    (append return-list
            (mapcar (lambda (node)
                      (make-node-dirent (name node) node))
                    (get-dir-entries node
                                     real-start
                                     (- (1- end) real-start))))))

(define-callback create-directory tree-translator
				 (node user name mode)
  (let ((new-stat (make-stat (stat node))))
    (setf (stat-get new-stat 'mode) mode)
    (add-entry node (make-dir name new-stat node))))

(define-callback remove-entry tree-translator
				 (node user name directory-p)
  (remove-dir-entry node name))
