
; dynamic variable *translator*
; this is used to run at the interface level
; the *translator* methods
(defvar *translator* nil)

; port of current authentication server
(defconstant +auth-server+ (getauth))

; port of the current exec server (for file_exec interface)
(defconstant +exec-server+ (file-name-lookup +servers-exec+))

(if (not (port-valid +auth-server+))
  (error "Could not get a valid port name to the authentication server"))
(if (not (port-valid +exec-server+))
  (error "Could not get a valid port name to the exec server"))
