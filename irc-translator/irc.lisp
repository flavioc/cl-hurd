#!/usr/bin/run-lisp-trans
;
; settrans -ac foo ./irc.lisp nickname server
;

(unless (= (length ext:*args*) 2)
  (error "You must pass a nickname and the server as arguments."))

(asdf:operate 'asdf:load-op 'irc-translator)

