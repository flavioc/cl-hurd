
include ../../defs.mk

TARGETS = error.so

LDFLAGS =

all: $(TARGETS)

%.so: %.o
	$(CC) -shared -Wl\
		-o $*.so $*.o $(LDFLAGS)

clean:
	rm -f $(TARGETS) *.o
