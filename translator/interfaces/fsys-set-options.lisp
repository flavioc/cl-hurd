
(in-package :hurd-translator)

(defun %get-string-list-options (data data-len)
  (let ((total-len 0))
    (loop until (eq total-len data-len)
          collect (let* ((str (foreign-string-to-lisp data))
                         (len (1+ (length str))))
                    (incf-pointer data len)
                    (incf total-len len)
                    str))))

(defun %split-options (item)
  "Split options, examples:
'readonly' -> 'readonly'
'max-files=5' -> ('max-files' '5')
"
  (let ((pos (position #\= item)))
    (cond
      ((null pos)
       item)
      (t
        (list (subseq item 0 pos)
              (let* ((value (subseq item (1+ pos)))
                     (converted (read-from-string value)))
                (if (symbolp converted)
                  value
                  converted)))))))

;; FIXME: do children when do-children eq T.
(def-fsys-interface :fsys-set-options ((fsys port)
                                       (reply port)
                                       (reply-type msg-type-name)
                                       (data :pointer)
                                       (data-len msg-type-number)
                                       (do-children :boolean))
  (with-lookup protid fsys
    (let* ((options-list (%get-string-list-options data data-len))
           (filtered-list ; Remove options without "--"
             (remove-if-not (lambda (item)
                              (and (> (length item) 2)
                                   (string= "--"
                                            (subseq item 0 2))))
                            options-list))
           (final-list ; Remove initial --
             (mapcar (lambda (item)
                       (%split-options (subseq item 2)))
                     filtered-list)))
      (warn "final-list ~s" final-list)
      (set-options *translator* final-list)
      t)))