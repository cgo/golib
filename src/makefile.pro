TEMPLATE    = lib
CONFIG     = warn_on debug
#CONFIG      = warn_on release qt
INCLUDEPATH = ../include
LIBS        = -lstdc++
#DEFINES     = USE_QT
SOURCES = 	data/goarray.cc \
		data/goheap.cc \
		data/gostring.cc \
		nifty/gorandom.cc \
		misc/godate.cc \
		financial/gopaper.cc \
		financial/godepot.cc \
		math/gonvector.cc \
		graphics/goima.cc \
		non-free/depot/godepotframe.cc \
		non-free/depot/gopaperframe.cc \
		non-free/qtwidgets/goinputwidget.cc \

HEADERS =	../include/goarray.h \
		../include/goheap.h \
		../include/gostring.h \
		../include/gorandom.h \
		../include/misc/godate.h \
		../include/financial/gopaper.h \
		../include/financial/godepot.h \
		../include/math/gonvector.h \
		../include/graphics/goima.h \
		../include/non-free/depot/godepotframe.h \
		../include/non-free/depot/gopaperframe.h \
		../include/non-free/qtwidgets/goinputwidget.h

OBJECTS = 	data/goarray.o \
		data/goheap.o \
		data/gostring.o \
		nifty/gorandom.o \
		misc/godate.o \
		financial/gopaper.o \
		financial/godepot.o \
		math/gonvector.o \
		graphics/goima.o

TARGET = Go
VERSION = 0.1
DESTDIR =	../lib





