
(setf *default-file-encoding* (make-encoding :charset 'charset:utf-8 :line-terminator :unix))

(defpackage :irc-translator
  (:use :cl :hurd-common :mach
        :hurd :hurd-translator
        :hurd-tree-translator
        :flexi-streams))

(in-package :irc-translator)

;; Load configuration.
;(assert (= (length ext:*args*) 1))
;(defconstant +file+ (first ext:*args*))
;(load +file+)
(defvar *server* "irc.freenode.org")
(defvar *nickname* "teste_nickname_n")
(defvar *start-channels* '("#linux" "#c++"))

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
               :documentation "Irc connection object.")))

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
(defclass conversation-entry (entry channel-obj-entry data-entry) ())

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
                 ((node conversation-entry) user start amount stream)
  (when (has-access-p node user :read)
    (read-from-data-entry node start amount stream)))

(define-callback write-file irc-translator
                 (node user offset stream)
                 (declare (ignore node user offset stream))
                 nil)

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
  (when (remove-dir-entry node name)
    (irc:part (connection *translator*)
              (irc:normalized-name (channel found)))
    t))

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

(define-callback fill-root-node irc-translator
                 ((node dir-entry))
  (setf (file-stat translator)
        (make-stat (stat node)
                   :mode (make-mode :perms '((:owner :read)
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
  (dolist (item *start-channels*)
    (irc:join (connection translator)
              item)))

(defmethod add-new-info ((node conversation-entry) str)
  (let* ((current-size (stat-get (stat node) 'st-size))
         (begin-p (zerop current-size))
         (this-size (1+ (length str)))
         (final-str (concatenate-string str (list #\Newline)))
         (new-size (+ current-size this-size)))
    (adjust-array (data node)
                  new-size
                  :fill-pointer t)
    (replace (data node) (string-to-octets final-str)
             :start1 current-size)
    (setf (stat-get (stat node) 'st-size) new-size)))

(defmethod add-new-info ((channel-name string) str)
  (let ((found (get-entry (root *translator*) channel-name)))
    (when (and found
               (typep found 'channel-entry))
      (let ((found2 (get-entry found "conversation")))
        (when (and found2
                   (typep found2 'conversation-entry))
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
          (conversation-entry (make-instance 'conversation-entry
                                           :parent channel-dir
                                           :stat (make-stat (file-stat *translator*))
                                           :data (make-array 0
                                                             :adjustable t
                                                             :fill-pointer t
                                                             :element-type '(unsigned-byte 8))
                                           :channel channel-obj)))
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
                  (with-output-to-string (s)
                    (format s "~s enters the room" who)))))

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
                  (with-output-to-string (s)
                    (format s "~s exits the room (~s)" who
                            (if (null (rest args))
                              "no reason"
                              (second args)))))))

(defun handle-privmsg (msg)
  (add-new-info (get-channel-name (first (irc:arguments msg)))
                (with-output-to-string (s)
                  (format s "~s: ~a"
                          (irc:source msg)
                          (second (irc:arguments msg))))))

(defun handle-irc-message (msg)
  (let ((cmd (irc:command msg)))
    (cond
      ((string= "JOIN" cmd)
       (handle-join msg))
      ((string= "PART" cmd)
       (handle-part msg))
      ((string= "PRIVMSG" cmd)
       (handle-privmsg msg))
      (t nil))))

(defun main ()
  (let ((translator
          (make-instance 'irc-translator
                         :name "irc-translator")))
    (setup-translator translator)
    (let ((*translator* translator))
      (loop do (progn
                 (wait :miliseconds 50)
                 (when (has-data-p (connection *translator*))
                     (handler-bind
                            ((irc:no-such-reply
                               #'(lambda (c)
                                   (declare (ignore c))
                                   (continue))))
                            (handle-irc-message (irc:read-message
                                                  (connection *translator*))))))))))

(main)

