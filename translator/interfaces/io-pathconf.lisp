
(def-io-interface :io-pathconf ((io port)
								(what pathconf-type)
								(value :pointer))
   (with-lookup protid io
	  (let ((result (pathconf *translator*
							  (get-node protid)
							  (get-user protid)
							  what)))
		(cond
		  ((null result)
		   :invalid-argument)
		  (t
			(setf (mem-ref value :int) result)
			t)))))
