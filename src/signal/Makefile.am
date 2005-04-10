# Example for library:
noinst_LTLIBRARIES = libGosignal.la
libGosignal_la_SOURCES = gosignal3dbase.cpp \
                         gosubsignal3d.cc \
                         gosignal3d.cc \
                         gosignal2d.cc \
                         gofilter3d.cpp \
                         godwt.cc \
                         gohaar3d.cpp \
                         godwt3d.cpp \
						 gofilter1d.cpp \
						 gosignalhelper.cpp \
						 gohistogram.cpp \
						 gosignal3diterator.cpp \
						 gosignal3dgenericiterator.cpp
                         
INCLUDES = -I@top_srcdir@/include 
#libgame_la_LIBADD = -L$(GOPATH)/lib -lGo

AM_CFLAGS = -D_REENTRANT
AM_CXXFLAGS = -D_REENTRANT

