#!/bin/sh
# Call this from one directory up from golib
tar -czvh --exclude=".svn" --exclude="CVS" --exclude=".libs" --exclude=".deps" --exclude="*.o" --exclude="*.so*" --exclude="*.a" --exclude="*.la" --exclude="doc/html" -f golib-`date +%Y%m%d`.tar.gz \
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
 golib/doc\
 golib/examples\
 golib/include\
 golib/maindocpage.h\
 golib/CMakeLists.txt\
 golib/src\
 golib/swig\
 golib/python\
 golib/matlab\
 golib/gui\
 golib/gl\
 golib/exp\
 golib/tools 
