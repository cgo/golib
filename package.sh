#!/bin/sh
# Call this from one directory up from golib
tar czvhf golib-`date +%Y%m%d`.tar.gz \
 golib/AUTHORS\
 golib/COPYING\
 golib/ChangeLog\
 golib/Doxyfile\
 golib/INSTALL\
 golib/Makefile.am\
 golib/Makefile.in\
 golib/NEWS\
 golib/README\
 golib/RELEASE\
 golib/aclocal.m4\
 golib/bootstrap\
 golib/config.guess\
 golib/config.status\
 golib/config.sub\
 golib/configure\
 golib/configure.in\
 golib/doc\
 golib/examples\
 golib/include\
 golib/install-sh\
 golib/libtool\
 golib/ltmain.sh\
 golib/maindocpage.h\
 golib/missing\
 golib/mkinstalldirs\
 golib/namespacedoc.h\
 golib/src\
 golib/tools --exclude="graphics" --exclude=".svn" --exclude=".libs" --exclude=".deps" --exclude="*.o" --exclude="*.so*" --exclude="*.a" --exclude="*.la"
