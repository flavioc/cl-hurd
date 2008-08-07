
(in-package :hurd-translator)

(defun %use-current-offset-p (offset)
  "If we should use the current node offset."
  (= -1 offset))

(defun %io-read-link (node user start out-stream)
  (let ((size (stat-get (stat node) 'st-size)))
    (cond
      ((> start size) t) ; Everything is read by now.
      (t
        (when (and (link node)
                   (allow-link-p *translator* node user))
          ; The link target is something and the translator writter allows links
          ; Write the link target to the stream.
          (write-sequence (string-to-octets (link node))
                          out-stream)
          t)))))

(defun %try-read (node user start amount out-stream)
  (case (get-type (stat node))
    (:lnk
      (%io-read-link node user start out-stream))
    (otherwise
      (read-file *translator*
                 node user start
                 amount
                 out-stream))))

(defconstant +default-chunk-size+ 2048 "Default size when amount is ANY amount.")

(defun %io-read (open-node node user amount offset)
  "Returns an array of bytes read and total, nil otherwise."
  (let* ((current-offset-p (%use-current-offset-p offset))
         (start (if current-offset-p
                  (file-offset open-node)
                  offset)))
    (with-stream (out-stream (make-in-memory-output-stream))
      (let ((err (%try-read node user start amount out-stream)))
        (when (eq err nil)
          (return-from %io-read :not-permitted))
        (let* ((data-read (get-output-stream-sequence out-stream))
               (total-read (length data-read)))
          (cond
            ((> total-read amount)
             :gratuitous-error)
            (t
              (if current-offset-p
                (incf (file-offset open-node) total-read))
              (values data-read total-read))))))))

(def-io-interface :io-read ((port port)
                            (data :pointer)
                            (datalen :pointer)
                            (offset off-t)
                            (amount vm-size))
  (with-lookup protid port
    (block io-read
           (let ((open-node (open-node protid))
                 (node (get-node protid))
                 (user (get-user protid)))
             (unless (flag-is-p (flags open-node) :read)
               (return-from io-read :bad-fd))
             (when (< offset -1)
               (return-from io-read :invalid-argument))
             (when (and (not (%use-current-offset-p offset))
                        (> offset (stat-get (stat node) 'st-size)))
               (return-from io-read :invalid-argument))
             (multiple-value-bind (data-read total)
               (%io-read open-node
                         node
                         user
                         (if (%is-minus-one-p amount)
                           +default-chunk-size+
                           amount)
                         offset)
               (when (null total)
                 (setf (mem-ref datalen 'msg-type-number) 0)
                 (return-from io-read
                              ; Possibly return an error code.
                              (if (null data-read)
                                t
                                data-read)))
               (let ((needs-allocate-p (>=
                                         total
                                         (mem-ref datalen 'msg-type-number))))
                 (when needs-allocate-p
                   ; We need to grow the pointer to copy all the data we have.
                   (setf (mem-ref data :pointer)
                         (mmap (null-pointer)
                               (1+ total)
                               '(:prot-read :prot-write)
                               '(:map-anon)
                               0 0)))
                 (setf (mem-ref datalen 'msg-type-number) total)
                 ; Finally, write data to the foreign pointer.
                 (lisp-string-to-foreign data-read
                                         (mem-ref data :pointer)
                                         (1+ total))
                 t))))))
