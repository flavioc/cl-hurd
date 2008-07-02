
(define-helper-library round-page)

(defcfun ("helper_round_page" %helper-round-page)
		 vm-offset
		 (address vm-offset))

(defun round-page (address)
  (%helper-round-page address))
