# $Id$

XMLS = test02plus03.xml


CONVERTER = ./scm2xml.scm
RM = rm


.SUFFIXES: .scm .xml

all: $(XMLS)

.scm.xml:
	$(CONVERTER) $< > $@

clean:
	- $(RM) -f $(XMLS)

