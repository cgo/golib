# Example for library:
noinst_LTLIBRARIES = libGomisc.la
libGomisc_la_SOURCES = goconfigfile.cc \
					   goconfigfileaccess.cc \
					   godate.cc \
					   goerror.cc \
					   goexception.cc \
					   gofileio.cc \
					   goobjectbase.cc \
					   goobjectinfo.cc \
					   gostatusobject.cc \
					   gotransferfunction.cc \
					   goobjectmessage.cc
INCLUDES = -I@top_srcdir@/include 
#libgame_la_LIBADD = -L$(GOPATH)/lib -lGo

AM_CFLAGS = -D_REENTRANT
AM_CXXFLAGS = -D_REENTRANT

