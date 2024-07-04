## Requirements
You'll need the following GNU tools:
	- autoconf
	- automake
	- make
	- pkg-config

## Maintainer Build
	1. aclocal                   (creates aclocal.m4)
	2. autoheader                (creates config.h.in)
	3. autoconf                  (creates configure & autom4te.cache)
	4. automake --add-missing    (creates Makefile.in)
Note:
	5. autoreconf                (remakes above files after changes)

## Building
	1. mkdir ../chf.build        (create build directory)
	2. cp README.build ../chf.build/README (detailed build instructions)
	3. cd ../chf.build           (change working directory for build)
	4. ../chf/configure          (creates Makefiles)
	5. make                      (builds entire package)
	6. make check                (run dialyzer and common_test)

## Installing
	1. sudo make install         (installs embedded application in system)

## Cleaning
	1. make clean                (removes files created with make)
	2. make distclean            (removes files created with configure also)

## Options
	../chf/configure --enable-debug

