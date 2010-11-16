NAME = Gauche-www-cgi-dispatch-tir
DIST_GENERATED = DIST_EXCLUDE_X configure VERSION \
                 src/lib/www/cgi/dispatch-tir/version.scm
CONFIG_GENERATED = config.log config.status autom4te*.cache
.PHONY: all clean test check install uninstall distclean maintainer-clean

all:
	cd src; $(MAKE) all

clean:
	rm -f core *~
	- cd src; $(MAKE) clean
	- cd test; $(MAKE) clean

test: check

check:
	cd test; $(MAKE) check

install: all
	cd src; $(MAKE) install

uninstall:
	cd src; $(MAKE) uninstall

distclean: clean
	rm -rf $(CONFIG_GENERATED)
	- cd src; $(MAKE) distclean
	- cd test; $(MAKE) distclean

maintainer-clean: clean
	rm -rf $(CONFIG_GENERATED) $(DIST_GENERATED)
	- cd src; $(MAKE) maintainer-clean
	- cd test; $(MAKE) maintainer-clean
