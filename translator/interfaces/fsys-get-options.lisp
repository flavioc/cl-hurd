
(in-package :hurd-translator)

(def-fsys-interface :fsys-get-options ((fsys port)
                                       (reply port)
                                       (reply-type msg-type-name)
                                       (data :pointer)
                                       (data-len :pointer))
  (%get-options-callback fsys data data-len))
