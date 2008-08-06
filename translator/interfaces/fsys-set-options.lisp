
(in-package :hurd-translator)

(def-fsys-interface :fsys-set-options ((fsys port)
                                       (reply port)
                                       (reply-type msg-type-name)
                                       (data :pointer)
                                       (data-len msg-type-number)
                                       (do-children :boolean))
  (declare (ignore reply reply-type))
  (when (port-exists-p fsys)
    (let ((new-options (get-foreign-options data data-len)))
      (when do-children
        (let (nodes-done)
          (bucket-iterate
            (port-bucket *translator*)
            (lambda (port)
              (when (typep port 'protid)
                (let ((node (get-node port)))
                  (when (and (box-active-p (box node))
                             (not (member node nodes-done)))
                    (fsys-set-options (box-fetch-control (box node))
                                      :options new-options
                                      :do-children t)
                    (push node nodes-done))))))))
      (set-options *translator* new-options))
    t))

