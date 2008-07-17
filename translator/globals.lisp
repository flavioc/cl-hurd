
(in-package :hurd-translator)

(defvar *translator* nil
  "Variable used to represent the currently used translator.")

;; Port of current authentication server.
(defconstant +auth-server+ (getauth))

(unless (port-valid +auth-server+)
  (error "Could not get a valid port name to the authentication server"))

;; Port of the current exec server (for file-exec callback)
(defconstant +exec-server+ (file-name-lookup +servers-exec+))

(unless (port-valid +exec-server+)
  (error "Could not get a valid port name to the exec server"))
