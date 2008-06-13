
(defclass open-node ()
  ((node :initarg :refers)
   (file-pos :initform 0)
   (lock-status :initform 'unlock) ; /usr/include/sys/file.h
   (openstat)
   (root-parent :initform nil)
   (shadow-root :initform nil)
   (shadow-root-parent :initform nil)))

