
(in-package :hurd-translator)

(defun %shutdown (flags)
  (when (and
          (flag-is-p flags :unlink)
          (is-dir-p (stat (root *translator*))))
    (return-from %shutdown :resource-busy))
  ; XXX make translators go away
  (when (and
          (not (flag-is-p flags :force))
          (plusp (bucket-count-type (port-bucket *translator*) 'protid)))
    (return-from %shutdown :resource-busy))
  (unless (flag-is-p flags :nosync)
    (file-syncfs *translator* (make-iouser-root) t t))
  (shutdown *translator*))

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
         #-(or sbcl clisp) (error "(quit) not implemented in your lisp implementation")
         )
        (t
          shut-ret)))))

