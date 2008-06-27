
(defcfun ("maptime_map" %maptime-map)
	 err
	 (use-mach-dev :boolean)
	 (dev-name :string)
	 (time-value :pointer))

(defcstruct mapped-time-value
	    (seconds :int)
	    (microseconds :int)
	    (check-seconds :int))

(defun maptime-map (&optional (use-mach-dev nil) (dev-name nil))
  (let ((ret (foreign-alloc :pointer :count 1)))
    (let ((error-code (%maptime-map use-mach-dev
				    dev-name
				    ret)))
      (select-error error-code
		    (mem-aref ret :pointer 0)))))

(defun maptime-seconds (ptr)
  (foreign-slot-value ptr 'mapped-time-value 'seconds))

(defun maptime-microseconds (ptr)
  (foreign-slot-value ptr 'mapped-time-value 'microseconds))

(defun maptime-check-seconds (ptr)
  (foreign-slot-value ptr 'mapped-time-value 'check-seconds))
