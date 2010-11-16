NAME = eval-sv
DIST_GENERATED = DIST_EXCLUDE_X configure VERSION \
                 lib/eval-sv/version.scm
CONFIG_GENERATED = config.log config.status autom4te*.cache
.PHONY: all clean test check install uninstall distclean maintainer-clean

all:
	$(MAKE) -C lib all

clean:
	rm -f core *~
	- $(MAKE) -C lib clean
	- $(MAKE) -C test clean

test: check

check:
	$(MAKE) -C test check

install: all
	$(MAKE) -C lib install

uninstall:
	$(MAKE) -C lib uninstall

distclean: clean
	rm -rf $(CONFIG_GENERATED)
	- $(MAKE) -C lib distclean
	- $(MAKE) -C test distclean

maintainer-clean: clean
	rm -rf $(CONFIG_GENERATED) $(DIST_GENERATED)
	- $(MAKE) -C lib maintainer-clean
	- $(MAKE) -C test maintainer-clean
