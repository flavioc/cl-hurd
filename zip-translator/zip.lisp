#!/usr/bin/run-lisp-trans

; settrans -ac foo ./zip.lisp zip-file.zip
;

(unless (= (length ext:*args*) 1)
  (error "You must pass the target directory path to zip."))

(asdf:operate 'asdf:load-op 'zip-translator)

