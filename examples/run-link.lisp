#!/usr/bin/run-lisp-trans
;
; settrans -ac foo ./run-link.lisp link-target
;

(unless (= (length ext:*args*) 1)
  (error "You must pass the link target as an argument."))

(asdf:operate 'asdf:load-op 'link-translator)

