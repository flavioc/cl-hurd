
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

(defun %create-data-array (size contents)
  (make-array size
              :initial-contents contents
              :adjustable t
              :fill-pointer t
              :element-type '(unsigned-byte 8)))

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
  ((contents :initarg :data
             :initform nil
             :accessor data)))

(defclass channel-obj-entry ()
  ((channel :initarg :channel
            :accessor channel)))

(defclass channel-entry (dir-entry channel-obj-entry) ())
(defclass topic-entry (data-entry entry channel-obj-entry) ())
(defclass users-entry (entry channel-obj-entry) ())

(defun update-topic-data (node)
  (setf (data node)
        (string-to-octets (concatenate-string
                            (irc:topic (channel node))
                            (list #\Newline)))))

(define-callback read-file irc-translator
                 ((node topic-entry) user start amount stream)
  (when (has-access-p node user :read)
    (when (null (data node))
      (update-topic-data node))
    (let* ((size (stat-get (stat node) 'st-size))
           (size-res (- size start)))
      (unless (plusp size-res)
        (return-from read-file t))
      (let* ((total (min size-res amount))
             (end (+ start total)))
        (write-sequence (subseq (data node) start end)
                        stream)
        t))))

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
        (loop for key being the hash-keys of users
              do (cond
                   ((>= pos end)
                    (return-from read-file t))
                   (t
                     (loop for a across (concatenate-string key
                                                            (list #\Newline))
                           do (progn
                                (cond
                                  ((>= pos end)
                                   (return-from read-file t))
                                  ((>= pos start)
                                   (write-byte (char-code a) stream)))
                                (incf pos))))))
        t))))

(defun %read-sequence (stream)
  (let ((arr (%create-data-array 0 '())))
    (read-sequence arr stream)
    arr))

(define-callback write-file irc-translator
                 (node user offset stream)
  (return-from write-file nil)
  (unless (has-access-p node user :write)
    (return-from write-file nil))
  (when (is-dir-p (stat node))
    (return-from write-file :is-a-directory))
  (let* ((size (stat-get (stat node) 'st-size))
         (arr (%read-sequence stream))
         (amount (length arr))
         (final-size (max (+ amount offset) size)))
    (unless (= final-size size)
      (adjust-array (data node)
                    final-size
                    :fill-pointer t))
    (loop for octet across arr
          for i from offset
          do (progn
               (setf (aref (data node) i) octet)))
    ; Update stat size.
    (setf (stat-get (stat node) 'st-size) final-size)
    t))

(define-callback report-no-users irc-translator
                 ((node data-entry))
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
                   :type :reg))
  (setf (dir-stat translator)
        (make-stat (stat node)
                   :mode (make-mode :perms '((:owner :read :exec)
                                             (:group :read :exec)))
                   :type :dir))
  (setf (connection translator)
        (irc:connect :nickname *nickname* :server *server*))
  (dolist (item *start-channels*)
    (irc:join (connection translator)
              item)))

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
                                      :channel channel-obj)))
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
      (create-new-channel orig-channel channel)))
  (warn "join arguments ~s ~s" (irc:arguments msg)
        (irc:source msg)))

(defun remove-channel (name)
  (remove-dir-entry (root *translator*) name))

(defun handle-part (msg)
  (let* ((orig-channel (first (irc:arguments msg)))
         (channel (get-channel-name orig-channel))
         (who (irc:source msg)))
    (when (string= who *nickname*)
      (remove-channel channel)))
  (warn "part arguments ~s ~s" (irc:arguments msg)
        (irc:source msg)))

(defun handle-irc-message (msg)
  (cond
    ((string= "JOIN" (irc:command msg))
     (handle-join msg))
    ((string= "PART" (irc:command msg))
     (handle-part msg))
    (t nil)))

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

