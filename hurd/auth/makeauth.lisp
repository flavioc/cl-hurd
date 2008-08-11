
(in-package :hurd)

(defcfun ("auth_makeauth" %auth-makeauth)
  err
  (handle auth-t)
  (other-handles :pointer)
  (other-handles-poly msg-type-name)
  (other-handles-len msg-type-number)
  (euids :pointer)
  (euids-len msg-type-number)
  (auids :pointer)
  (auids-len msg-type-number)
  (egids :pointer)
  (egids-len msg-type-number)
  (agids :pointer)
  (agids-len msg-type-number)
  (new-handle :pointer))

(defun auth-makeauth (auth other-auths
                           other-auth-poly
                           eff-user
                           avail-user)
  "Create a new authentication handle."
  (let ((euids (get-foreign-uids eff-user))
        (egids (get-foreign-gids eff-user))
        (auids (get-foreign-uids avail-user))
        (agids (get-foreign-gids avail-user))
        (total (length other-auths)))
    (with-cleanup (free-memory-list (list euids egids auids agids))
      (with-foreign-pointer (new-handle (foreign-type-size 'auth-t))
        (with-foreign-pointer (other-handles-ptr (* total
                                                   (foreign-type-size 'auth-t)))
          (loop for i from 0 below total
                for handle in other-auths
                do (setf (mem-aref other-handles-ptr 'auth-t i)
                         handle))
          (let ((err (%auth-makeauth auth other-handles-ptr 
                                     other-auth-poly
                                     total
                                     (first euids) (second euids)
                                     (first auids) (second auids)
                                     (first egids) (second egids)
                                     (first agids) (second agids)
                                     new-handle)))
            (select-error err
                          (mem-ref new-handle 'auth-t))))))))

