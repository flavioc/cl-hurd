
(in-package :hurd-tree-translator)

(defclass sorted-container ()
  ((table :accessor table
          :initform (make-hash-table :test 'equal)
          :documentation "Hash table for fast lookup.")
   (ls :initform nil
       :accessor sorted-list
       :documentation "List that keeps elements sorted")
   (indexer :initform nil
            :accessor indexer
            :initarg :indexer
            :documentation "Function that returns the key of an element.")
   (sorter-fn :initform nil
              :initarg :sorter
              :accessor sorter
              :documentation "Function that sorts two elements."))
  (:documentation "Container with fast lookups (O(1)) and fast element sorting (worst case O(N))."))

(defun make-sorted-container (sorter indexer)
  "Creates a new sorted container."
  (make-instance 'sorted-container
                 :sorter sorter
                 :indexer indexer))

(defmethod count-elements ((container sorted-container))
  "Returns the number of elements in the container."
  (hash-table-count (table container)))

(defmethod insert-element ((container sorted-container) element)
  "Inserts an element into the container."
  ; First insert the element in the hash-table.
  (setf (gethash (funcall (indexer container) element)
                 (table container))
        element)
  ; Now insert it on the sorted list.
  (setf (sorted-list container)
        (%insertion-sort (sorted-list container)
                        (indexer container)
                        (sorter container)
                        element))
  element)

(defun %insertion-sort (ls indexer sorter element)
  (cond
    ((null ls)
     (list element))
    (t
      (if (funcall sorter
                   (funcall indexer element)
                   (funcall indexer (first ls)))
        (cons element ls)
        (cons (first ls)
              (%insertion-sort (rest ls) indexer sorter element))))))

(defmethod elements-from ((container sorted-container) start n-elements)
  "Returns n sorted elements starting at 'start'."
  (unless (plusp n-elements)
    (return-from elements-from nil))
  (subseq (sorted-list container) start (+ start n-elements)))

(defmethod remove-element ((container sorted-container) key)
  "Removes an element with key 'key'."
  ; First, remove it from the hash table.
  (remhash key (table container))
  ; Now, from the sorted list.
  (setf (sorted-list container)
        (delete key
                (sorted-list container)
                :key (indexer container)
                :test #'equal))
  container)

(defmethod iterate-elements ((container sorted-container) fun)
  "Runs 'fun' for each key-value pair."
  (maphash fun (table container)))

(defmethod get-element ((container sorted-container) key)
  "Gets an element using 'key'."
  (gethash key (table container)))

;(defvar *a* (make-sorted-container #'string< #'first))
;(insert-element *a* (list "a" 2))
;(insert-element *a* (list "c" 5))
;(insert-element *a* (list "b" 3))
;(insert-element *a* (list "z" 7))
;(print (count-elements *a*))
;(print (sorted-list *a*))
;(print (elements-from *a* 0 2))
;(print (elements-from *a* 1 1))
;(print (elements-from *a* 0 3))
;(remove-element *a* "g")
;(remove-element *a* "c")
;(print (count-elements *a*))
;(print (sorted-list *a*))
