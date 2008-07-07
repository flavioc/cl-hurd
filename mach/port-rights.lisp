
(in-package :mach)

(defcfun ("mach_port_mod_refs" %mach-port-mod-refs)
	 err
	 (task ipc-space)
	 (port-name port)
	 (right port-right)
	 (delta port-delta))

(defun port-mod-refs (port right
                           &optional
                           (delta -1) ; drop 1 ref by default
                           (task (task-self)))
  "Modify the specified port right's count of user references."
  (%mach-port-mod-refs task port right delta))

; kern_return_t mach_port_get_refs (ipc_space_t task, mach_port_t name, mach_port_right_t right, mach_port_urefs_t *refs)

(defcfun ("mach_port_get_refs" %mach-port-get-refs)
  err
  (task ipc-space)
  (name port)
  (right port-right)
  (refs :pointer))

(defun port-get-refs (port-name right &optional (task (task-self)))
  "Get number of references of a specific right in a port name."
  (with-foreign-pointer (refs (foreign-type-size 'port-urefs))
    (let ((return-code
            (%mach-port-get-refs task
                                 port-name
                                 right
                                 refs)))
      (select-error return-code
                    (mem-ref refs 'port-urefs)))))

