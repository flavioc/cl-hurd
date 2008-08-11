
(in-package :mach)

(defcfun ("mach_port_type" %mach-port-type)
  err
  (task ipc-space)
  (name port)
  (ptype :pointer))

(defun port-type (port &optional (task (task-self)))
  "Return the characteristics of the target port name."
  (with-foreign-object (ptype 'port-type-t)
    (let ((return-code
            (%mach-port-type task port ptype)))
      (select-error return-code (mem-ref ptype 'port-type-t)))))

(defcfun ("mach_port_names" %mach-port-names)
  err
  (task ipc-space)
  (names :pointer)
  (ncount :pointer)
  (types :pointer)
  (tcount :pointer))

(defun port-names (&optional (task (task-self)))
  "Get a list with port names and the associated port type for the port names in task."
  (with-foreign-pointer (names (foreign-type-size :pointer))
    (with-foreign-pointer (ncount (foreign-type-size :pointer))
      (with-foreign-pointer (types (foreign-type-size :pointer))
        (with-foreign-pointer (tcount (foreign-type-size :pointer))
          (let ((return-code
                  (%mach-port-names task
                                    names
                                    ncount
                                    types
                                    tcount)))
            (select-error return-code
                          (let ((names-list (%port-names-to-list names ncount))
                                (types-list (%port-types-to-list types tcount)))
                            ;; The GNU Mach reference manual says we
                            ;; should free the newly allocated memory
                            (munmap (mem-ref names :pointer)
                                    (* (mem-ref ncount 'msg-type-number)
                                       (foreign-type-size 'port)))
                            (munmap (mem-ref types :pointer)
                                    (* (mem-ref tcount 'msg-type-number)
                                       (foreign-type-size 'port-type-t)))
                            (mapcar #'cons
                                    names-list types-list)))))))))

(defun %port-names-to-list (names-addr ncount)
  "Transforms a foreign array of port names into a list."
  (loop for i from 0 below (mem-ref ncount 'msg-type-number)
        collect (mem-aref names-addr 'port i)))

(defun %port-types-to-list (types-addr tcount)
  "Transforms a foreign array of port types into a list."
  (loop for i from 0 below (mem-ref tcount 'msg-type-number)
        collect (mem-aref types-addr 'port-type-t i)))

(defcfun ("mach_port_rename" %mach-port-rename)
  err
  (task ipc-space)
  (old port)
  (new port))

(defun port-rename (old-name new-name &optional (task (task-self)))
  "Renames the port name 'old-name' to 'new-name'. Returns 'new-name' on success, nil otherwise."
  (let ((return-code
          (%mach-port-rename task old-name new-name)))
    (select-error return-code
                  new-name)))
