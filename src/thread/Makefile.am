# Example for library:
noinst_LTLIBRARIES = libGothread.la
libGothread_la_SOURCES = gothread.cc \
						 goconsumer.cc \
						 goproducer.cc \
						 gosynchronized.cc \
						 gothreadobject.cc
INCLUDES = -I@top_srcdir@/include 
#libgame_la_LIBADD = -L$(GOPATH)/lib -lGo

AM_CFLAGS = -D_REENTRANT
AM_CXXFLAGS = -D_REENTRANT

