
(in-package :hurd-translator)

(defmacro %add-callback (name args doc &body body)
  "Add a new API classback function."
  `(progn
     (defgeneric ,name (translator ,@args)
                 (:documentation ,doc))
     (defmethod ,name ((translator translator) ,@args)
       ,@(if (null body)
           'nil 
           body))))

(%add-callback make-root-node (underlying-stat)
  "Called when the translator wants to create the root node.
'underlying-stat' refers to the stat structure from the file
where the translator is being set up."
  (make-instance 'node :stat underlying-stat))

(%add-callback pathconf (node user what)
  "This is called when the io-pathconf RPC is called.
'what' refers to the type of information the user wants,
please see common/pathconf.lisp."
	(case what
    ((:link-max :max-canon :max-input
                :pipe-buf :vdisable :sock-maxbuf)
     -1)
    ((:name-max)
     1024)
    ((:chown-restricted :no-trunc)
     1)
    ((:prio-io :sync-io :async-io)
     0)
    (:filesizebits
      32)))

(%add-callback allow-open-p (node user flags is-new-p)
  "'user' wants to open 'node' with flags 'flags', 'is-new-p' indicates that this is a newly created node. This should return nil when we don't wanna open the node.")

(%add-callback get-translator (node)
  "This must return the translator path that is set under 'node'.")

(%add-callback file-chmod (node user mode)
  "The user is attempting to 'chmod' node with the mode permission bits.")

(%add-callback file-chown (node user uid gid)
  "The user is attempting to 'chown' node with uid and gid.")

(%add-callback file-utimes (node user atime mtime)
  "The user is attempting to change the access and modification time of the node.
'atime' or 'mtime' can be :now.
Using (setf (stat-get (stat node) 'mtime) mtime) will do it for you in both cases.")

(%add-callback dir-lookup (node user filename)
  "This must return the node with the name 'filename' in the directory 'node', nil when it is not found.")

(%add-callback create-file (node user filename mode)
  "The user wants to create a file on the directory 'node' with name 'filename' and mode 'mode'.")

(%add-callback number-of-entries (node user)
  "This must return the number of entries in the directory 'node' from the 'user' point of view."
  0)

(%add-callback get-entries (node user start end)
  "This sould return a list of dirent objects representing the contents of the directory 'node' from 'start' to 'end' (index is zero based).")

(%add-callback allow-author-change (node user author)
  "User wants to change the file's author, return t if it is ok, nil otherwise.")

(%add-callback create-directory (node user name mode)
  "The user wants to create a directory in the directory 'node' with 'name' and 'mode', return nil if don't permitted.")

(%add-callback remove-directory-entry (node user name directory-p)
  "The user wants to remove an entry named 'name' from the directory 'node'. 'directory-p' indicates that the entry is a directory.")

(%add-callback file-read (node user start amount stream)
  "User wants to read 'amount' bytes starting at 'start'. These bytes should be written to the stream 'stream'. Return t in case of success, nil otherwise.")

(%add-callback file-sync (node user wait-p omit-metadata-p)
  "User wants to sync the contents in node. 'wait-p' indicates the user wants to wait. 'omit-metadata-p' indicates we must omit the update of the file metadata (like stat information).")

(%add-callback file-syncfs (user wait-p do-children-p)
  "User wants to sync the entire filesystem. 'wait-p' indicates the user wants to wait for it. 'do-children-p' indicates we should also sync the children nodes."
  t)

(%add-callback file-write (node user offset stream)
  "The user wants to write the bytes in the input stream 'stream' starting at 'offset'.")

(%add-callback drop-node (node)
  "The 'node' has no more references, drop it."
  (warn "Dropped node ~s" node)
  nil)

(%add-callback report-access (node user)
  "This should return a list of permitted access modes for 'user'.Permitted modes are:
:read :write :exec."
  nil)

(%add-callback refresh-statfs (user)
  "The statfs translator field must be updated for 'user'.
Return t for success, nil for unsupported operation."
  nil)

(%add-callback file-change-size (node user new-size)
  "The user wants to change node size to 'new-size'.
Return t on success, nil for unsupported operation."
  nil)

(%add-callback file-rename (user old-dir old-name new-dir new-name)
  "Rename file 'old-name' from 'old-dir' to 'new-name' in 'new-dir'.
Return T for success, nil for unsupported, or other error code for other errors."
  nil)

(%add-callback shutdown ()
  "Shutdown the translator."
  t)

(%add-callback create-anonymous-file (node user mode)
  "Create an anonymous file related to directory 'node'.
Return nil for unsupported operation.")

(%add-callback create-hard-link (dir user node name)
  "Create an hard link in directory with 'name' pointing to 'node'.")

(%add-callback block-read (node user)
  "Block until we can read data from node.
Return T when this is possible, nil otherwise."
  t)

(%add-callback block-write (node user)
  "Block until we can write data to node.
Return T when this is possible, nil otherwise."
  t)

(%add-callback get-options ()
  "Return a list of translator options similar to --arguments."
  (if (null (options translator))
    nil
    (get-translator-options (options translator))))

(%add-callback set-options (option-list)
  "Define a new set of translator options. 'option-list' is a list with two kinds of elements:
a string: defines a new activated option, something like --option.
a list of two elements: a string representing the option and the value, representing the option value. Option values can be strings, integers, nils, etc."
  (set-translator-options (options translator) option-list)
  ; Inform translator about option changes.
  (options-changed translator)
  t)

(%add-callback options-changed ()
  "Indicates that translator options have changed. You don't need to implement this if you implement 'set-options'.")

(%add-callback create-symlink (node user target)
  "Turn 'node' into a symlink to 'target'."
  nil)

(%add-callback allow-link-p (node user)
  "Return T to allow reading from the symlink 'node' to 'user'."
  t)

(%add-callback create-block (node user device)
  "Turn 'node' into a block device with device-id 'device'."
  nil)

(%add-callback create-character (node user device)
  "Turn 'node' into a character device with device-id 'device'."
  nil)

(%add-callback create-fifo (node user)
  "Turn 'node' into a fifo."
  nil)

(%add-callback create-socket (node user)
  "Turn 'node' into a socket."
  nil)

(%add-callback set-translator (node user arg-list)
  "Set passive translator 'arg-list' on 'node'."
  nil)

(defmacro define-callback (name trans-type args &body body)
  "Defines one the api callbacks defined above."
  `(defmethod ,name ((translator ,trans-type) ,@args)
     ,@body))

