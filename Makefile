#
#
#

CHICKEN_COMPILER=csc -X r7rs

LINK_REPOS=\
-r ../snow2/r7rs-srfis \
-r ../snow2/snow2-packages/snow \
-r ../snow2/snow2-packages/seth \
-r ../snow2/seth-misc

REPO="http://seth-misc.s3-website-us-east-1.amazonaws.com/index.scm"

LIBS=\
'(srfi 37)' '(snow bytevector)' '(seth string-read-write)' \
'(seth cout)' '(seth binary-pack)' '(seth port-extras)' '(seth flex)'


all:

flex-table-to-scm: flex-table-to-scm-chicken.scm
	$(CHICKEN_COMPILER) $^ -o $@

libs:
	snow2 -p $(REPO) install $(LIBS)

link-deps:
	snow2 -s $(LINK_REPOS) install $(LIBS)

install: flex-table-to-scm
	cp $^ /usr/local/bin/flex-table-to-scm

clean:
	rm -rf seth snow srfi seth-tests srfi-tests *~
