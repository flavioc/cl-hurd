
(in-package :hurd-translator)

(defvar *translator* nil
  "Variable used to represent the currently used translator.")

(defconstant +auth-server+ (getauth) "Port to the current authentication server.")

(unless (port-valid-p +auth-server+)
  (error "Could not get a valid port name to the authentication server"))

(defconstant +exec-server+ (file-name-lookup +servers-exec+) "Port to the current exec server.")

(unless (port-valid-p +exec-server+)
  (error "Could not get a valid port name to the exec server"))

