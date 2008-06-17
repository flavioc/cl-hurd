
CFLAGS = -Wall -Wextra -D_GNU_SOURCE=1
LDFLAGS = -lmachuser -lc -lhurduser

DEFS_DIRECTORY = /usr/include/hurd
MIG = mig -prefix lisp_ -user /dev/null -header /dev/null -server
