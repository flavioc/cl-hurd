
TARGETS = error.so

CFLAGS = -Wall -Wextra -D_GNU_SOURCE=1
LDFLAGS =

all: $(TARGETS)

%.so: %.o
	$(CC) -shared -Wl\
		-o $*.so $*.o $(LDFLAGS)

clean:
	rm -f $(TARGETS) *.o
