
(in-package :hurd-translator)

(defun %has-node-p (table node)
  (multiple-value-bind (foo found-p)
    (gethash node table)
    (declare (ignore foo))
    found-p))

(defun %put-node (table node)
  (setf (gethash node table) nil)
  table)

(defun %create-new-protid (open-node user node flags newnode-p)
  "Creates a new protid."
  (let ((new-flags (disable-flags flags +open-flags+)))
    (when (allow-open-p *translator* node user new-flags newnode-p)
      (when (flag-is-p flags :trunc)
        (unless (file-change-size *translator* node user 0)
          (return-from %create-new-protid :not-permitted)))
      (let* ((new-user (make-iouser :old user))
             (new-open-node (make-open-node
                              node
                              new-flags
                              :copy open-node))
             (new-protid
               (new-protid *translator*
                           new-user
                           new-open-node)))
        (values :retry-normal
                ""
                (get-right new-protid)
                :make-send)))))

(defun %must-handle-shadow-roots (open-node node this-path)
  (and (or (eq (root *translator*) node)
           (eq (shadow-root open-node) node))
       (string= this-path "..")
       (or (eq node (shadow-root open-node))
           (port-valid-p (root-parent open-node)))))

(defun %handle-shadow-roots (open-node node rest-path)
  (cond
    ((eq node (shadow-root open-node))
     (values :retry-reauth
             (if (null rest-path)
               ""
               (join-path rest-path))
             (shadow-root-parent open-node)
             :copy-send))
    ((port-valid-p (root-parent open-node))
     (values :retry-reauth
             (if (null rest-path)
               ""
               (join-path rest-path))
             (root-parent open-node)
             :copy-send))))

(defun %handle-symlinks (open-node user dir node rest-path flags mode table)
  (let ((target (link node)))
    (cond
      ((eq (char target 0) #\/) ; Points to root /!
       (values :retry-magical
               (concatenate-string target
                                   "/"
                                   (join-path rest-path))
               nil
               :make-send))
      (t
        ; Lookup new path based on the symlink target.
        (%dir-lookup open-node
                     user
                     dir
                     (if (null rest-path)
                       (split-path target)
                       (append (remove "" (split-path target) :test #'string=)
                               rest-path))
                     (disable-flags flags :creat)
                     mode
                     (%put-node table node))))))

(defun %must-handle-symlink (node user flags rest-path)
  (and node
       (is-lnk-p (stat node))
       (link node)
       (allow-link-p *translator* node user)
       (or rest-path
           (and (not (flag-is-p flags :nolink))
                (not (flag-is-p flags :notrans))))))

(defun %must-handle-translator (node flags rest-path)
  (and (or (not (flag-is-p flags :notrans))
           rest-path) ; This is not the path end, so we must continue
       (box-translated-p (box node))))

(defun %dir-lookup (open-node user node path-ls flags mode table)
  (let ((this-path (first path-ls))
        (rest-path (rest path-ls)))
    (when (string= this-path "") ; this is last path
      (return-from %dir-lookup
                   (%create-new-protid open-node user node flags nil)))
    (when (%must-handle-shadow-roots open-node node this-path)
      (return-from %dir-lookup
                   (%handle-shadow-roots open-node node rest-path)))
    (let ((found-node (directory-lookup *translator* node user this-path)))
      (cond
        (found-node ; File exists.
          (when (%must-handle-translator found-node flags rest-path)
            (let* ((empty-user (make-empty-iouser))
                   (new-open-node (make-open-node node
                                                  nil
                                                  :copy open-node))
                   (protid (new-protid *translator* empty-user new-open-node))
                   (*current-dotdot* (get-send-right protid))
                   (*current-node* found-node))
              (multiple-value-bind (retry retry-name port)
                (fetch-root (box found-node)
                            *current-dotdot*
                            (if rest-path flags nil)
                            user
                            #'get-translator-callback
                            (callback fetch-root-callback))
                (unless (or (eq retry :no-such-file)
                            (null retry))
                  (return-from %dir-lookup
                               (values retry
                                       (concatenate-string retry-name
                                                           "/"
                                                           (join-path rest-path))
                                       port
                                       :move-send))))))
          (cond
            ((and (flag-is-p flags :creat)
                  (flag-is-p flags :excl))
             :file-exists)
            ((%must-handle-symlink found-node user flags rest-path)
             (if (%has-node-p table found-node)
               :too-many-links
               (%handle-symlinks open-node user node found-node rest-path flags mode table)))
            ((null rest-path)
             (%create-new-protid open-node user found-node flags nil))
            ((and rest-path
                  (not (is-dir-p (stat found-node))))
             :not-directory)
            (t
              (%dir-lookup open-node user found-node rest-path flags mode table))))
          ; File does not exist.
          (t
            (cond
              ((and (flag-is-p flags :creat)
                    (null rest-path))
               (set-vtx mode nil)
               (set-spare mode nil)
               (set-type mode :reg)
               (let ((new-node (create-file *translator*
                                            node
                                            user
                                            this-path
                                            mode)))
                 (unless new-node
                   (return-from %dir-lookup :not-permitted))
                 (%create-new-protid open-node user new-node flags t)))
              (t
                :no-such-file)))))))

(defun %do-dir-lookup (filename protid flags mode)
  (cond
    ((and (not (string= "" filename))
          (eq (char filename 0) #\/))
     (values :retry-magical
             filename
             nil
             :make-send))
    (t
      (%dir-lookup (open-node protid)
                   (get-user protid)
                   (get-node protid)
                   (split-path filename)
                   flags
                   mode
                   (make-hash-table)))))

(def-fs-interface :dir-lookup ((dir-port port)
                               (filename :string)
                               (flags open-flags)
                               (mode mode-t)
                               (do-retry :pointer)
                               (retry-name :pointer)
                               (retry-port port-pointer)
                               (retry-port-type :pointer))
  (with-lookup dir-protid dir-port
    (multiple-value-bind (ret-do-retry
                           ret-retry-name
                           ret-retry-port
                           ret-retry-port-type)
      (%do-dir-lookup filename
                      dir-protid
                      flags
                      mode)
      (cond
        ((null ret-retry-name) ret-do-retry) ;; Some error ocurred
        (t
          (setf (mem-ref do-retry 'retry-type) ret-do-retry)
          (lisp-string-to-foreign ret-retry-name
                                  retry-name
                                  (1+ (length ret-retry-name)))
          (setf (mem-ref retry-port 'port) ret-retry-port)
          (setf (mem-ref retry-port-type 'msg-type-name) ret-retry-port-type)
          t)))))

