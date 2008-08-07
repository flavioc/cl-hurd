
TARGET_DIRS = libs launcher

all:
	@for dir in $(TARGET_DIRS); do \
		$(MAKE) -C $$dir; \
	done

clean:
	@for dir in $(TARGET_DIRS); do \
		$(MAKE) -C $$dir clean; \
	done

install:
	ln -sf $(PWD)/{hurd-common,mach,hurd,hurd-translator,tree-translator}.asd /usr/share/common-lisp/systems
