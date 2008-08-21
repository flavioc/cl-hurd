
(defpackage :irc-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator
        :flexi-streams))

(in-package :irc-translator)

(unless (= (length ext:*args*) 2)
  (error "You must pass a nickname and the server as arguments."))

(defvar *nickname* (first ext:*args*))
(defvar *server* (second ext:*args*))

(defconstant +max-file-size+ 5000)

(defun create-adjustable-array (&key (size 0) (contents nil))
  (make-array size
              :adjustable t
              :fill-pointer t
              :element-type '(unsigned-byte 8)
              :initial-contents contents))

(defun has-data-p (connection)
  (let ((result (socket:socket-status (irc:network-stream connection))))
    (or (eq :input result)
        (eq :io result))))

(defclass irc-translator (tree-translator)
  ((file-stat :initarg :file-stat
              :initform nil
              :accessor file-stat
              :documentation "Stat information for regular files.")
   (dir-stat :initarg :dir-stat
             :initform nil
             :accessor dir-stat
             :documentation "Stat information for directories.")
   (connection :initarg :connection
               :initform nil
               :accessor connection
               :documentation "Irc connection object.")
   (notice-node :initform nil
                :accessor notice-node
                :documentation "Node with notices from the server.")))

(defclass data-entry ()
  ((contents :initform nil
             :initarg :data
             :accessor data)))

(defclass channel-obj-entry ()
  ((channel :initarg :channel
            :accessor channel)))

(defclass channel-entry (dir-entry channel-obj-entry) ())
(defclass topic-entry (data-entry entry channel-obj-entry) ())
(defclass users-entry (entry channel-obj-entry) ())
(defclass log-entry (entry channel-obj-entry data-entry) ())
(defclass notice-entry (log-entry) ())
(defclass kick-entry (channel-obj-entry entry) ())
(defclass pvt-entry (log-entry)
  ((user :initarg :user
         :accessor user)))

(defun update-topic-data (node)
  (setf (data node)
        (string-to-octets (concatenate-string
                            (irc:topic (channel node))
                            (list #\Newline)))))

(defun read-from-data-entry (node start amount stream)
  (let* ((size (stat-get (stat node) 'st-size))
         (size-res (- size start)))
    (unless (plusp size-res)
      (return-from read-from-data-entry t))
    (let* ((total (min size-res amount))
           (end (+ start total)))
      (write-sequence (subseq (data node) start end)
                      stream)
      t)))

(define-callback read-file irc-translator
                 ((node topic-entry) user start amount stream)
  (when (has-access-p node user :read)
    (when (null (data node))
      (update-topic-data node))
    (read-from-data-entry node start amount stream)))

(define-callback read-file irc-translator
                 ((node kick-entry) user start amount stream)
  (declare (ignore start amount stream))
  (has-access-p node user :read)) ;; Nothing to read at all.

(defun get-key-list (hashtable)
  (sort (loop for key being the hash-keys of hashtable
              collect key)
        #'string<))

(define-callback read-file irc-translator
                 ((node users-entry) user start amount stream)
  (when (has-access-p node user :read)
    (let* ((users (irc:users (channel node)))
           (size (calculate-users-size users))
           (size-res (- size start)))
      (unless (plusp size-res)
        (return-from read-file t))
      (let* ((total (min size-res amount))
             (end (+ start total))
             (pos 0))
        (loop for nick in (get-key-list users)
              do (cond
                   ((>= pos end)
                    (return-from read-file t))
                   (t
                     (loop for a across (concatenate-string nick
                                                            (list #\Newline))
                           do (progn
                                (cond
                                  ((>= pos end)
                                   (return-from read-file t))
                                  ((>= pos start)
                                   (write-byte (char-code a) stream)))
                                (incf pos))))))
        t))))

(define-callback read-file irc-translator
                 ((node log-entry) user start amount stream)
  (when (has-access-p node user :read)
    (read-from-data-entry node start amount stream)))

(define-callback write-file irc-translator
                 ((node notice-entry) user offset stream amount)
  (declare (ignore user offset stream amount))
  nil)

(defun get-message-stream (stream amount)
  (let ((array (make-array amount :element-type '(unsigned-byte 8))))
    (read-sequence array stream)
    (string-trim (list #\Newline)
                 (octets-to-string array))))

(define-callback write-file irc-translator
                 ((node pvt-entry) user offset stream amount)
  (declare (ignore offset))
  (when (has-access-p node user :write)
    (let ((msg (get-message-stream stream amount)))
      (irc:privmsg (connection translator)
                   (user node)
                   msg)
      (add-new-info node
                    (make-privmsg-string *nickname* msg))
      t)))

(define-callback write-file irc-translator
                 ((node log-entry) user offset stream amount)
  (declare (ignore offset))
  (when (has-access-p node user :write)
    (let ((msg (get-message-stream stream amount)))
      (irc:privmsg (connection *translator*)
                   (channel node)
                   msg)
      (add-new-info node
                    (make-privmsg-string *nickname* msg))
      t)))

(define-callback write-file irc-translator
                 ((node kick-entry) user offset stream amount)
  (declare (ignore offset))
  (when (has-access-p node user :write)
    (let* ((kick-str (string-trim " " (get-message-stream stream amount)))
           (space-pos (position #\Space kick-str))
           (reason-p (not (null space-pos))))
      (let ((nick (if reason-p (subseq kick-str 0 space-pos) kick-str))
            (reason (if reason-p (string-trim " " (subseq kick-str (1+ space-pos)))
                      "no reason")))
        (irc:kick (connection *translator*)
                  (channel node)
                  (irc:find-user (connection *translator*)
                                 nick)
                  reason)
        t))))

(define-callback report-no-users irc-translator
                 ((node topic-entry))
  (setf (data node) nil))

(defun calculate-users-size (table)
  (loop for key being the hash-keys of table
        sum (1+ (length key))))

(define-callback refresh-node irc-translator
                 ((node topic-entry) user)
  (declare (ignore user))
  (setf (stat-get (stat node) 'st-size)
        (1+ (length (irc:topic (channel node))))))

(define-callback refresh-node irc-translator
                 ((node users-entry) user)
  (declare (ignore user))
  (let* ((users (irc:users (channel node)))
         (size (calculate-users-size users)))
    (setf (stat-get (stat node) 'st-size) size)))

(defmethod do-remove-directory-entry ((found node) node name)
  (remove-dir-entry node name))

(defmethod do-remove-directory-entry ((found channel-entry) node name)
  (declare (ignore node))
  (irc:part (connection *translator*)
            (irc:normalized-name (channel found))
            (format nil "rm ~a" name))
  t)

(define-callback remove-directory-entry irc-translator
                 (node user name)
  (let ((found (get-entry node name)))
    (when (and found
               (is-owner-p found user))
      (do-remove-directory-entry found node name))))

(define-callback create-directory irc-translator
                 (node user name mode)
  (declare (ignore mode))
  (unless (eq node (root *translator*))
    (return-from create-directory nil))
  (unless (is-owner-p node user)
    (return-from create-directory nil))
  (let ((old (get-entry node name)))
    (cond
      (old nil)
      (t
        (irc:join (connection *translator*)
                  (concatenate-string "#"
                                      name))
        t))))

(define-callback shutdown irc-translator
                 ()
  (irc:quit (connection *translator*) "settrans -g")
  (sleep 0.5))

(defun make-pvt-file (root user)
  (let ((new-entry (make-instance 'pvt-entry
                                  :parent root
                                  :stat (make-stat (file-stat *translator*))
                                  :data (create-adjustable-array)
                                  :user user)))
    (add-entry root new-entry user)))

(define-callback create-file irc-translator
                 (node user filename mode)
  (declare (ignore user mode))
  (when (eq node (root translator))
    (make-pvt-file (root translator)
                   filename)))

(define-callback fill-root-node irc-translator
                 ((node dir-entry))
  (setf (file-stat translator)
        (make-stat (stat node)
                   :mode (make-mode :perms '((:owner :read :write)
                                             (:group :read)))
                   :type :reg
                   :size 0))
  (setf (dir-stat translator)
        (make-stat (stat node)
                   :nlink 0
                   :mode (make-mode :perms '((:owner :read :exec)
                                             (:group :read :exec)))
                   :type :dir))
  (setf (connection translator)
        (irc:connect :nickname *nickname* :server *server*))
  (let ((notice-entry (make-instance 'notice-entry
                                     :parent node
                                     :stat (make-stat (file-stat translator))
                                     :data (create-adjustable-array))))
    (setf (notice-node translator) notice-entry)
    (add-entry node notice-entry "notice")))

(defmethod add-new-info ((node log-entry) str)
  (let* ((current-size (stat-get (stat node) 'st-size))
         (final-str (concatenate-string str (list #\Newline)))
         (this-size (length final-str))
         (new-size (+ current-size this-size)))
    (adjust-array (data node)
                  new-size
                  :fill-pointer t)
    (replace (data node) (string-to-octets final-str)
             :start1 current-size)
    (when (> new-size +max-file-size+)
      (let ((extra (- new-size +max-file-size+)))
      (setf (data node)
            (create-adjustable-array :size +max-file-size+
                                     :contents (subseq (data node) extra)))
      (decf new-size extra)))
    (setf (stat-get (stat node) 'st-size) new-size)))

(defmethod add-new-info ((channel-name string) str)
  (let ((found (get-entry (root *translator*) channel-name)))
    (when (and found
               (typep found 'channel-entry))
      (let ((found2 (get-entry found "conversation")))
        (when (and found2
                   (typep found2 'log-entry))
          (add-new-info found2 str))))))

(defun create-new-channel (orig-channel channel)
  (let* ((channel-obj (irc:find-channel (connection *translator*)
                                        orig-channel))
         (channel-dir (make-instance 'channel-entry
                                     :parent (root *translator*)
                                     :stat (make-stat (dir-stat *translator*))
                                     :channel channel-obj)))
    (assert (not (null channel-obj)))
    (add-entry (root *translator*) channel-dir channel)
    (let ((topic-entry (make-instance 'topic-entry
                                      :parent channel-dir
                                      :stat (make-stat (file-stat *translator*))
                                      :channel channel-obj))
          (users-entry (make-instance 'users-entry
                                      :parent channel-dir
                                      :stat (make-stat (file-stat *translator*))
                                      :channel channel-obj))
          (conversation-entry (make-instance 'log-entry
                                           :parent channel-dir
                                           :stat (make-stat (file-stat *translator*))
                                           :data (create-adjustable-array)
                                           :channel channel-obj))
          (kick-entry (make-instance 'kick-entry
                                     :parent channel-dir
                                     :stat (make-stat (file-stat *translator*))
                                     :channel channel-obj)))
      (add-entry channel-dir kick-entry "kick")
      (add-entry channel-dir conversation-entry "conversation")
      (add-entry channel-dir users-entry "users")
      (add-entry channel-dir topic-entry "topic"))))

(defun get-channel-name (str)
  (string-left-trim "#"
                    (string-downcase str)))

(defun handle-join (msg)
  (let* ((orig-channel (first (irc:arguments msg)))
         (channel (get-channel-name orig-channel))
         (who (irc:source msg)))
    (when (string= who *nickname*)
      (create-new-channel orig-channel channel))
    (add-new-info channel
                  (format nil "~s enters the room" who))))

(defun remove-channel (name)
  (remove-dir-entry (root *translator*) name))

(defun handle-part (msg)
  (let* ((args (irc:arguments msg))
         (orig-channel (first (irc:arguments msg)))
         (channel (get-channel-name orig-channel))
         (who (irc:source msg)))
    (when (string= who *nickname*)
      (remove-channel channel))
    (add-new-info channel
                  (format nil "~s exits the room (~s)" who
                          (if (null (rest args))
                            "no reason"
                            (second args))))))

(defun make-privmsg-string (who msg)
  (format nil "~s: ~a" who msg))

(defun handle-privmsg-pvt (source msg)
  (add-new-info (if (has-entry-p (root *translator*) source)
                  (get-entry (root *translator*) source)
                  (make-pvt-file (root *translator*) source))
                (make-privmsg-string source msg)))

(defun handle-privmsg (msg)
  (let ((dest (first (irc:arguments msg)))
        (src (irc:source msg))
        (str (second (irc:arguments msg))))
    (cond
      ((string= dest *nickname*)
       (handle-privmsg-pvt src str))
      (t
        (add-new-info (get-channel-name dest)
                      (make-privmsg-string src str))))))

(defun join-string-list (string-list)
  "Concatenates a list of strings and puts spaces between the elements."
  (format nil "~{~A~^ ~}" string-list))

(defun handle-notice (msg)
  (add-new-info (notice-node *translator*)
                (join-string-list (irc:arguments msg))))

(defun handle-quit (msg)
  (add-new-info (notice-node *translator*)
                (format nil "QUIT: ~a ~a"
                        (irc:source msg)
                        (join-string-list (irc:arguments msg)))))

(defun handle-kick (msg)
  (let* ((args (irc:arguments msg))
         (orig-channel (first (irc:arguments msg)))
         (channel (get-channel-name orig-channel))
         (kicker (irc:source msg))
         (kicked (second args))
         (reason (if (= (length args) 3) (third args) "no reason")))
    (add-new-info channel
                  (format nil "~s kicks ~s from the room (~s)"
                          kicker
                          kicked
                          reason))
    (when (string= kicked *nickname*)
      (remove-channel channel)
      (add-new-info (notice-node *translator*)
                    (format nil "KICK: You got kicked from ~s by ~s (~s)"
                            orig-channel
                            kicker
                            reason)))))

(defun handle-irc-message (msg)
  (let ((cmd (irc:command msg)))
    (cond
      ((string= "JOIN" cmd)
       (handle-join msg))
      ((string= "PART" cmd)
       (handle-part msg))
      ((string= "PRIVMSG" cmd)
       (handle-privmsg msg))
      ((or (string= "NOTICE" cmd)
           (and (>= (length cmd) 3)
                (or (string= "ERR" (subseq cmd 0 3))
                    (string= "RPL" (subseq cmd 0 3)))))
       (handle-notice msg))
      ((string= "QUIT" cmd)
       (handle-quit msg))
      ((string= "KICK" cmd)
       (handle-kick msg))
      ((or (string= "PING" cmd)
           (string= "UNKNOWN-REPLY" cmd))))))

(defun main ()
  (let ((translator
          (make-instance 'irc-translator
                         :name "irc-translator")))
    (setup-translator translator)
    (let ((*translator* translator))
      (loop do (progn
                 (wait :miliseconds 100)
                 (loop while (has-data-p (connection *translator*))
                       do (handler-bind
                            ((irc:no-such-reply
                               #'(lambda (c)
                                   (declare (ignore c))
                                   (continue))))
                            (handle-irc-message (irc:read-message
                                                  (connection *translator*))))))))))

(main)

