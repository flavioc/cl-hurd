
(in-package :hurd-tree-translator)

;;
;; This file contains a special kind of translator:
;; the tree-translator
;; It implements without much hassle the directory callbacks
;; and maintains a structured directory hierarchy.
;;

(defclass tree-translator (translator)
  ((name :initform "tree-translator"
         :documentation "Translator name"))
  (:documentation "The tree-translator."))

(defmethod propagate-read-to-execute ((stat stat))
  "Enables the execute permission bit if the read bit is on."
  (if (has-perms-p stat :read :owner)
    (set-perms stat :exec :owner))
  (if (has-perms-p stat :read :group)
    (set-perms stat :exec :group))
  (if (has-perms-p stat :read :others)
    (set-perms stat :exec :others))
  t)

;; It ensures the root node is a directory
;; and calls the function fill-root-node to fill the directory
;; structure.
(define-callback make-root-node tree-translator
                 (underlying-node underlying-stat)
  (declare (ignore underlying-node))
  (when (not (is-dir-p underlying-stat))
    (propagate-read-to-execute underlying-stat))
  (set-trans underlying-stat nil)
  (let ((obj (make-instance 'dir-entry
                            :stat underlying-stat)))
    (setf (stat-get (stat obj) 'st-nlink) 2)
    (fill-root-node translator obj)
    obj))

(define-callback fill-root-node tree-translator
                 ((root dir-entry))
  "This should be used to construct the directory structure. 'root' is the newly created
root node."
  nil)

(define-callback directory-lookup tree-translator
				 (node user filename)
  (unless (has-access-p node user :read)
    (return-from directory-lookup nil))
  (let ((found (cond
                ((string= filename ".") node)
                ((string= filename "..") (parent node))
                (t (get-entry node filename)))))
    (when (and found
               (has-access-p found user :read))
      found)))

(define-callback number-of-entries tree-translator
				 (node user)
  (cond
    ((has-access-p node user :read)
     (dir-size node))
    (t 0)))

(defun not-permitted-entries-p (name)
  (or (string= name ".")
      (string= name "..")))

(define-callback get-entries tree-translator
				(node user start end)
  (unless (has-access-p node user :read)
    (return-from get-entries nil))
  (let* (return-list
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
  (when (not-permitted-entries-p name)
    (return-from create-directory nil))
  (unless (is-owner-p node user)
    (return-from create-directory nil))
  (let ((old (get-entry node name)))
    (cond
      (old nil)
      (t
        (add-entry node (make-instance
                          'dir-entry
                          :stat (make-stat (stat node) :mode mode)
                          :parent node)
                   name)))))

(define-callback remove-directory-entry tree-translator
				 (node user name)
  (when (not-permitted-entries-p name)
    (return-from remove-directory-entry nil))
  (let ((found (get-entry node name)))
    (when found
      (when (is-owner-p found user)
        (cond
          ((and (is-dir-p (stat found))
                (plusp (- (dir-size found) 2)))
           :directory-not-empty)
          (t
            (remove-dir-entry node name)))))))

(define-callback create-hard-link tree-translator
                 (dir user file name)
  (when (not-permitted-entries-p name)
    (return-from create-hard-link nil))
  (when (is-owner-p dir user)
    (add-entry dir file name)
    t))

(define-callback file-rename tree-translator
                 (user old-dir old-name new-dir new-name)
  (when (not-permitted-entries-p new-name)
    (return-from file-rename nil))
  (let ((old-entry (get-entry old-dir old-name)))
    (when (and (is-owner-p old-entry user)
               (is-owner-p new-dir user)
               (has-access-p new-dir user :write))
      (rename-dir-entry old-dir old-name new-dir new-name t)
      t)))

