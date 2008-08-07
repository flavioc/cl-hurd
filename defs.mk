
CFLAGS = -Wall -Wextra -D_GNU_SOURCE=1 -DPIC -fPIC \
				 -std=gnu99 -fgnu89-inline -D_FILE_OFFSET_BITS=64

LDFLAGS = -lmachuser -lc -lhurduser

HURD_DIRECTORY = /usr/include/hurd
MACH_DIRECTORY = /usr/include/mach
MIG_SERVER = mig -prefix lisp_S_ -user /dev/null -header /dev/null -server
MIG_USER = mig -server /dev/null -header /dev/null -user

