
(in-package :hurd-translator)

(defun %shutdown (flags)
  (when (and
          (fsys-goaway-flag-is-p flags :unlink)
          (is-dir-p (stat (root *translator*))))
    (return-from %shutdown :resource-busy))
  ; XXX make translators go away
  (when (and
          (not (fsys-goaway-flag-is-p flags :force))
          (plusp (bucket-total-users (port-bucket *translator*))))
    (return-from %shutdown :resource-busy))
  (if (fsys-goaway-flag-is-p flags :nosync)
    t
    (file-syncfs *translator* (make-iouser-root) t t)))

;;
;; Run the fsys-goaway callback terminating the translator.
;;
(def-fsys-interface :fsys-goaway ((control port)
                                  (reply port)
                                  (reply-type msg-type-name)
                                  (flags fsys-goaway-flags))
  (with-lookup protid control
    (let ((shut-ret (%shutdown flags)))
      (cond
        ((eq t shut-ret)
         (fsys-goaway-reply reply reply-type t)
         #+clisp (ext:exit)
         #+sbcl (sb-ext:quit :unix-status 0)
         #-(or sbcl clisp) (error "(exit) not implemented in your lisp implementation")
         )
        (t
          shut-ret)))))

