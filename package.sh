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
 golib/bootstrap\
 golib/configure\
 golib/configure.in\
 golib/doc\
 golib/examples\
 golib/include\
 golib/maindocpage.h\
 golib/missing\
 golib/mkinstalldirs\
 golib/namespacedoc.h\
 golib/CMakeLists.txt\
 golib/src\
 golib/swig\
 golib/tools --exclude="graphics" --exclude=".svn" --exclude=".libs" --exclude=".deps" --exclude="*.o" --exclude="*.so*" --exclude="*.a" --exclude="*.la"
