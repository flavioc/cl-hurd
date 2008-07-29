#!/bin/sh

cd examples
settrans --timeout=0 -a ../foo /usr/bin/clisp ../test.lisp $*
