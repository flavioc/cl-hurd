
include ../../defs.mk

TARGETS = error.so portset-demuxer.so \
		  fetch-root.so \
		  round-page.so \
			file-utimes.so

LDFLAGS =

all: $(TARGETS)

fetch-root.so: fetch-root.o
	$(CC) -shared -Wl \
		-o fetch-root.so fetch-root.o $(LDFLAGS) -lfshelp

%.so: %.o
	$(CC) -shared -Wl\
		-o $*.so $*.o $(LDFLAGS)

clean:
	rm -f *.so *.o
