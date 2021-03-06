
(in-package :hurd-common)

;; This file maps the mapped_time_value_t type to Lisp.

;; Load libshouldbeinlibc
(defun load-hurd-common-libraries ()
  (define-foreign-library libshouldbeinlibc
                          (:unix (:or "libshouldbeinlibc.so.0.3" "libshouldbeinlibc.so"))
                          (t (:default "libshouldbeinlibc")))
  (use-foreign-library libshouldbeinlibc))

(load-hurd-common-libraries)

(defcfun ("maptime_map" %maptime-map)
  err
  (use-mach-dev :boolean)
  (dev-name :string)
  (time-value :pointer))

(defcstruct mapped-time-value
  "Mapped time value. Can be found at mach/time_value.h."
  (seconds :int)
  (microseconds :int)
  (check-seconds :int))

(defun maptime-map (&optional (use-mach-dev nil) (dev-name nil))
  "Return a mapped time pointer or nil and error in case of errors.
Returned value is a foreign pointer."
  (with-foreign-pointer (ret (foreign-type-size :pointer))
    (let ((error-code (%maptime-map use-mach-dev
                                    dev-name
                                    ret)))
      (select-error error-code
                    (mem-ref ret :pointer)))))

(defun maptime-seconds (ptr)
  "Return the seconds field from a mapped-time-value."
  (foreign-slot-value ptr 'mapped-time-value 'seconds))

(defun maptime-microseconds (ptr)
  "Return the microseconds field from a mapped-time-value."
  (foreign-slot-value ptr 'mapped-time-value 'microseconds))

(defun maptime-check-seconds (ptr)
  "Return the check seconds field from a mapped-time-value."
  (foreign-slot-value ptr 'mapped-time-value 'check-seconds))

(define-symbol-macro *mapped-time* (maptime-map))

