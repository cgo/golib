# Example for library:
noinst_LTLIBRARIES = libGosignal.la
libGosignal_la_SOURCES = godwt.cc \
						 gofilter.cc \
						 gofilterbank.cc \
						 gofilterbankreverse.cc \
						 gofilterbankstage.cc \
						 gofilterbankstagereverse.cc \
						 gofilterfir.cc \
						 gofiltergauss.cc \
						 gojoystick.cc \
						 gonavdevice.cc \
						 gonavkeyboard.cc \
						 goresample.cc \
						 gosignal.cc \
						 gosignal2d.cc \
						 gosignal3d.cc \
						 gosignalstat.cc \
						 gosignaltransform.cc \
						 gosubsignal3d.cc
INCLUDES = -I@top_srcdir@/include 
#libgame_la_LIBADD = -L$(GOPATH)/lib -lGo

AM_CFLAGS = -D_REENTRANT
AM_CXXFLAGS = -D_REENTRANT

