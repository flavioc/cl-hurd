
(defclass entry ()
  ((this-node :initform nil)
   (name :initform nil)
   (stat) ; FIXME
   (timestamp :initform nil) ; FIXME
   ))

(defclass dir (entry)
   ((entries :initform nil) ;FIXME: an hash table
   ))
