#!/bin/sh
# Call this from one directory up from golib
tar czvhf golib-`date +%Y%m%d`.tar.gz \
 golib/AUTHORS\
 golib/COPYING\
 golib/ChangeLog\
 golib/Doxyfile\
 golib/INSTALL\
 golib/Makefile.am\
 golib/package.sh\
 golib/NEWS\
 golib/README\
 golib/RELEASE\
 golib/bootstrap\
 golib/configure.in\
 golib/doc\
 golib/examples\
 golib/include\
 golib/maindocpage.h\
 golib/CMakeLists.txt\
 golib/src\
 golib/swig\
 golib/matlab\
 golib/gui\
 golib/exp\
 golib/tools --exclude=".svn" --exclude="CVS" --exclude=".libs" --exclude=".deps" --exclude="*.o" --exclude="*.so*" --exclude="*.a" --exclude="*.la" --exclude="doc/html"
