
TARGET_DIRS = libs launcher docs

all:
	@for dir in $(TARGET_DIRS); do \
		$(MAKE) -C $$dir; \
	done

clean:
	@for dir in $(TARGET_DIRS); do \
		$(MAKE) -C $$dir clean; \
	done

install: all
	ln -sf $(PWD)/{hurd-common,mach,hurd,hurd-translator,tree-translator,hurd-streams,unzip-translator}.asd \
		/usr/share/common-lisp/systems
	make -C libs install
	make -C launcher install
