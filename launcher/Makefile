
include ../defs.mk

CFLAGS += -DNDEBUG

TARGET = run-lisp-trans

all: $(TARGET)

run-lisp-trans: run-lisp-trans.o

clean:
	rm -f $(TARGET) *.o

install: run-lisp-trans
	install -m 555 run-lisp-trans /usr/bin
