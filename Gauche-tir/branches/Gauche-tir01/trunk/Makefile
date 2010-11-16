DIST_GENERATED = DIST_EXCLUDE_X configure VERSION tir/version.scm
CONFIG_GENERATED = config.log config.status autom4te*.cache
.PHONY: all clean test check install uninstall distclean maintainer-clean

all:
	cd tir; $(MAKE) all

clean:
	rm -f core *~
	- cd tir; $(MAKE) clean
	- cd test; $(MAKE) clean

test: check

check:
	cd test; $(MAKE) check

install: all
	cd tir; $(MAKE) install

uninstall:
	cd tir; $(MAKE) uninstall

distclean: clean
	rm -rf $(CONFIG_GENERATED)
	- cd tir; $(MAKE) distclean
	- cd test; $(MAKE) distclean

maintainer-clean: clean
	rm -rf $(CONFIG_GENERATED) $(DIST_GENERATED)
	- cd tir; $(MAKE) maintainer-clean
	- cd test; $(MAKE) maintainer-clean
