#!/usr/bin/run-lisp-trans
;
; settrans -ac foo ./run-mux.lisp file-list command
;

(unless (= (length ext:*args*) 2)
  (error "Argument syntax: <file list> <classification command>"))

(asdf:operate 'asdf:load-op 'mux-translator)

