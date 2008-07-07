
(in-package :mach)

;;
;; In this file we put foreign types that are so
;; simple that they don't desert a single file.
;;

(defctype task port "task_t type")

(defctype ipc-space task "ipc_space_t type")

(defctype port-pointer :pointer "A pointer to a mach_port_t")

(defctype port-mscount :unsigned-int "mach_port_msgcount_t type")

(defctype port-seqno :unsigned-int "mach_port_seqno_t type")

(defctype msg-seqno :unsigned-int "mach_msg_seqno_t type")

(defctype port-delta :int "mach_port_delta_t type")

(defctype msg-type-number :unsigned-int "mach_msg_type_number_t type")

(defctype vm-size :unsigned-int "vm_size_t type")

(defctype vm-offset :unsigned-int "vm_offset_t type")

(defctype vm-address :unsigned-int "vm_address_t type")

(defctype msg-size :unsigned-int "mach_msg_size_t type")

(defctype msg-timeout :unsigned-int "mach_msg_timeout_t type")

(defctype vm-task port "vm_task_t type")

(defctype port-urefs :unsigned-int "mach_port_urefs_t type")

(defctype port-msgcount :unsigned-int "mach_port_msgcount_t type")

(defctype port-rights :unsigned-int "mach_port_rights_t type")

