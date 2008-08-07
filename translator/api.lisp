
(in-package :hurd-translator)

(defmacro %add-callback (name args doc &body body)
  "Add a new API classback function."
  `(progn
     (defgeneric ,name (translator ,@args)
                 (:documentation ,doc))
     (defmethod ,name ((translator translator) ,@args)
       ,@(if (null body)
           (list `(declare (ignore translator ,@args)))
          body))))

(%add-callback make-root-node (underlying-node underlying-stat)
  "Called when the translator wants to create the root node.
'underlying-stat' refers to the stat structure from the file
where the translator is being set up. 'underlying-node' is the port to that file."
  (declare (ignore underlying-node))
  (make-instance 'node :stat underlying-stat))

(%add-callback pathconf (node user what)
  "This is called when the io-pathconf RPC is called.
'what' refers to the type of information the user wants,
please see common/pathconf.lisp."
  (declare (ignore translator node user))
	(case what
    ((:link-max :max-canon :max-input
                :pipe-buf :vdisable :sock-maxbuf)
     -1)
    ((:name-max) 1024)
    ((:chown-restricted :no-trunc) 1)
    ((:prio-io :sync-io :async-io) 0)
    (:filesizebits 32)))

(%add-callback allow-open-p (node user flags is-new-p)
  "'user' wants to open 'node' with flags 'flags', 'is-new-p' indicates that this is a newly created node. This should return nil when we don't wanna open the node."
  (declare (ignore is-new-p))
  (when (flag-is-p flags :read)
    (unless (has-access-p node user :read)
      (return-from allow-open-p nil)))
  (when (flag-is-p flags :write)
    (unless (has-access-p node user :write)
      (return-from allow-open-p nil)))
  (when (flag-is-p flags :exec)
    (unless (has-access-p node user :exec)
      (return-from allow-open-p nil)))
  t)

(%add-callback chmod-file (node user mode)
  "The user is attempting to 'chmod' node with the mode permission bits."
  (cond
    ((is-owner-p node user)
     (copy-perms mode (stat node))
     t)
    (t nil)))

(%add-callback chown-file (node user uid gid)
  "The user is attempting to 'chown' node with uid and gid."
  (cond
    ((is-owner-p node user)
     (when (valid-id-p uid)
       (setf (stat-get (stat node) 'st-uid) uid))
     (when (valid-id-p gid)
       (setf (stat-get (stat node) 'st-gid) gid))
     t)
    (t nil)))

(%add-callback utimes-file (node user atime mtime)
  "The user is attempting to change the access and modification time of the node.
'atime' or 'mtime' can be +now-time-value+.
Using (setf (stat-get (stat node) 'st-mtime) mtime) will do it for you in both cases."
  (cond
    ((is-owner-p node user)
     (when atime
       (setf (stat-get (stat node) 'st-atime) atime))
     (when mtime
       (setf (stat-get (stat node) 'st-mtime) mtime))
     t)
    (t nil)))

(%add-callback directory-lookup (node user filename)
  "This must return the node with the name 'filename' in the directory 'node', nil when it is not found.")

(%add-callback create-file (node user filename mode)
  "The user wants to create a file on the directory 'node' with name 'filename' and mode 'mode'.")

(%add-callback number-of-entries (node user)
  "This must return the number of entries in the directory 'node' from the 'user' point of view."
  (declare (ignore node user))
  0)

(%add-callback get-entries (node user start end)
  "This sould return a list of dirent objects representing the contents of the directory 'node' from 'start' to 'end' (index is zero based).")

(%add-callback allow-author-change-p (node user author)
  "User wants to change the file's author, return t if it is ok, nil otherwise."
  (declare (ignore author))
  (is-owner-p node user))

(%add-callback create-directory (node user name mode)
  "The user wants to create a directory in the directory 'node' with 'name' and 'mode', return nil if don't permitted.")

(%add-callback remove-directory-entry (node user name directory-p)
  "The user wants to remove an entry named 'name' from the directory 'node'. 'directory-p' indicates that the entry is a directory.")

(%add-callback read-file (node user start amount stream)
  "User wants to read 'amount' bytes starting at 'start'. These bytes should be written to the stream 'stream'. Return t in case of success, nil otherwise.")

(%add-callback sync-file (node user wait-p omit-metadata-p)
  "User wants to sync the contents in node. 'wait-p' indicates the user wants to wait. 'omit-metadata-p' indicates we must omit the update of the file metadata (like stat information)."
  (declare (ignore translator node user wait-p omit-metadata-p))
  t)

(%add-callback sync-fs (user wait-p do-children-p)
  "User wants to sync the entire filesystem. 'wait-p' indicates the user wants to wait for it. 'do-children-p' indicates we should also sync the children nodes."
  (declare (ignore translator user wait-p do-children-p))
  t)

(%add-callback write-file (node user offset stream)
  "The user wants to write the bytes in the input stream 'stream' starting at 'offset'.")

(%add-callback drop-node (node)
  "The 'node' has no more references, drop it."
  (declare (ignore translator node))
  ;(warn "Dropped node ~s" node)
  nil)

(%add-callback report-access (node user)
  "This should return a list of permitted access modes for 'user'.Permitted modes are:
:read :write :exec."
  (let ((ret))
    (when (has-access-p node user :read)
      (push :read ret))
    (when (has-access-p node user :write)
      (push :write ret))
    (when (has-access-p node user :exec)
      (push :exec ret))
    ret))

(%add-callback refresh-statfs (user)
  "The statfs translator field must be updated for 'user'.
Return t for success, nil for unsupported operation."
  (declare (ignore translator user))
  t)

(%add-callback file-change-size (node user new-size)
  "The user wants to change node size to 'new-size'.
Return t on success, nil for unsupported operation."
  (declare (ignore translator node user new-size))
  t)

(%add-callback file-rename (user old-dir old-name new-dir new-name)
  "Rename file 'old-name' from 'old-dir' to 'new-name' in 'new-dir'.
Return T for success, nil for unsupported, or other error code for other errors.")

(%add-callback shutdown ()
  "Shutdown the translator."
  (declare (ignore translator))
  t)

(%add-callback create-anonymous-file (node user mode)
  "Create an anonymous file related to directory 'node'.
Return nil for unsupported operation.")

(%add-callback create-hard-link (dir user node name)
  "Create an hard link in directory with 'name' pointing to 'node'.")

(%add-callback block-read (node user)
  "Block until we can read data from node.
Return T when this is possible, nil otherwise."
  (declare (ignore translator node user))
  t)

(%add-callback block-write (node user)
  "Block until we can write data to node.
Return T when this is possible, nil otherwise."
  (declare (ignore translator node user))
  t)

(%add-callback set-options (new-options)
  "Define a new set of translator options."
  (setf (options translator) new-options)
  ; Inform translator about option changes.
  (options-changed translator)
  t)

(%add-callback options-changed ()
  "Indicates that translator options have changed. You don't need to implement this if you implement 'set-options'.")

(%add-callback create-symlink (node user target)
  "Turn 'node' into a symlink to 'target'."
  (when (is-owner-p node user)
    (setf (link node) target)
    t))

(%add-callback allow-link-p (node user)
  "Return T to allow reading from the symlink 'node' to 'user'."
  (has-access-p node user :read))

(%add-callback create-block (node user device)
  "Turn 'node' into a block device with device-id 'device'.")

(%add-callback create-character (node user device)
  "Turn 'node' into a character device with device-id 'device'.")

(%add-callback create-fifo (node user)
  "Turn 'node' into a fifo.")

(%add-callback create-socket (node user)
  "Turn 'node' into a socket.")

(%add-callback refresh-node (node user)
  "'node' will be accessed by 'user'.")

(%add-callback report-seek (node user new-offset)
  "'user' seek the file 'node' to 'new-offset'.")

(defmacro define-callback (name trans-type args &body body)
  "Defines one the api callbacks defined above."
  `(defmethod ,name ((translator ,trans-type) ,@args)
     ,@body))

