
(in-package :hurd-translator)

(def-fs-interface :file-get-fs-options ((file port)
                                        (data :pointer)
                                        (data-len :pointer))
  (%get-options-callback file data data-len))

