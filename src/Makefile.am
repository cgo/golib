SUBDIRS = data math misc network nifty signal system thread types
# graphics math misc \
#		  network network nifty signal \
#		  system thread
lib_LTLIBRARIES = libGo.la
libGo_la_LDFLAGS = -release 0.3.0
libGo_la_LIBADD = data/libGodata.la \
				  math/libGomath.la \
				  misc/libGomisc.la \
				  network/libGonetwork.la \
				  nifty/libGonifty.la \
				  signal/libGosignal.la \
				  system/libGosystem.la \
				  thread/libGothread.la \
				  types/libGotypes.la
libGo_la_SOURCES = dummy.cc
