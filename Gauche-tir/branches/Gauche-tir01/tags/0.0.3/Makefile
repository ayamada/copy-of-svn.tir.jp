DIST_GENERATED = DIST_EXCLUDE_X configure VERSION lib/tir/version.scm
CONFIG_GENERATED = config.log config.status autom4te*.cache
.PHONY: all clean test check install uninstall distclean maintainer-clean

all:
	cd lib; $(MAKE) all

clean:
	rm -f core *~
	- cd lib; $(MAKE) clean
	- cd test; $(MAKE) clean

test: check

check:
	cd test; $(MAKE) check

install: all
	cd lib; $(MAKE) install

uninstall:
	cd lib; $(MAKE) uninstall

distclean: clean
	rm -rf $(CONFIG_GENERATED)
	- cd lib; $(MAKE) distclean
	- cd test; $(MAKE) distclean

maintainer-clean: clean
	rm -rf $(CONFIG_GENERATED) $(DIST_GENERATED)
	- cd lib; $(MAKE) maintainer-clean
	- cd test; $(MAKE) maintainer-clean
