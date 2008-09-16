#!/usr/bin/run-lisp-trans

; settrans -ac foo ./mod.lisp spec-file.lisp
;

(unless (= (length ext:*args*) 1)
  (error "You must pass the spec file as an argument."))

(asdf:operate 'asdf:load-op 'mod-translator)

