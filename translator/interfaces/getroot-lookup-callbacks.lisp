
(in-package :hurd-translator)

(defvar *current-node* nil)
(defvar *current-dotdot* nil)

(defun get-translator-callback (box)
  (let ((node (node box)))
    (cond
      ((box-passive-p (box node))
       (values (passive (box node))
               (stat-get (stat node) 'st-uid)
               (stat-get (stat node) 'st-gid)))
      (t :no-such-file))))

(defcallback fetch-root-callback
  err
  ((flags open-flags)
              (underlying port-pointer)
              (underlying-type :pointer))
  (let* ((node *current-node*)
         (stat (stat node))
         (user (make-iouser :uids (stat-get stat 'st-uid)
                            :gids (stat-get stat 'st-gid)))
         (new-open-node (make-open-node node flags
                                        :root-parent *current-dotdot*))
         (new (new-protid *translator* user new-open-node)))
    (setf (mem-ref underlying 'port) (get-right new))
    (setf (mem-ref underlying-type 'msg-type-name) :make-send)
    t))

