
include ../defs.mk

TARGETS = portset-demuxer.so \
		  fetch-root.so \
		  round-page.so \
			file-utimes.so \
			file-exec.so \
			exec-reauth.so

LDFLAGS =

all: $(TARGETS)

fetch-root.so: fetch-root.o
	$(CC) -shared -Wl \
		-o fetch-root.so fetch-root.o $(LDFLAGS) -lfshelp

exec-reauth.so: exec-reauth.o
	$(CC) -shared -Wl \
		-o exec-reauth.so exec-reauth.o $(LDFLAGS) -lshouldbeinlibc

%.so: %.o
	$(CC) -shared -Wl\
		-o $*.so $*.o $(LDFLAGS)

clean:
	rm -f *.so *.o
