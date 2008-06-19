
; dynamic variable *translator*
; this is used to run at the interface level
; the *translator* methods
(defvar *translator* nil)

; port name of current authentication server
(defconstant +auth-server+ (getauth))

(if (not (port-valid +auth-server+))
  (error "Could not get a valid port name to the authentication server"))
