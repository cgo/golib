#Makefile automatically created by mkmakefile (C)Christian Gosch
#Edited. Do not delete.

CXX		= gcc
CXXARGS		= -c -O2 -D_REENTRANT -DHAVE_CONFIG_H -Wno-non-template-friend
MOC		= $(QTDIR)/bin/moc
UIC		= $(QTDIR)/bin/uic

LD		= gcc
LDARGS		=  -L$(GOPATH)/lib -lGo -L$(QTDIR)/lib -lqt-mt -lSDL

LIBS		=  

INCLUDE		=  -I$(GOPATH)/include -I$(QTDIR)/include -I. -I/usr/local/include/SDL

TARGET		= renderdemo

%.o: %.cxx
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
%.o: %.cpp
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
%.o: %.cc
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
%_h.cc: %.h
	$(MOC) $< -o $@
%.h: %.ui
	$(UIC) $< -o $@

SOURCEDIRS = .
SOURCES = main.cc \
	  renderapplication.cc \
	  rendergui.cc \
	  transferfunctionframe.cc \
	  rendermainwindow.cc 
#	  posdisplay.cc
#	  navigationframe.cc
#SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cxx))
#SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cpp))
#SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cc))
OBJECTS0 = $(SOURCES)
OBJECTS0 := $(subst .cxx,.o,$(OBJECTS0))
OBJECTS1 := $(subst .cpp,.o,$(OBJECTS0))
OBJECTS2 := $(subst .cc,.o,$(OBJECTS1))
OBJECTS = $(OBJECTS2)

UIC_SOURCES := rendergui.ui
#UIC_HEADERS := $(subst .ui,.h,$(UIC_SOURCES))
#UIC_CC      := $(subst .ui,.cc,$(UIC_SOURCES))
MOC_HEADERS := rendergui.h \
		renderapplication.h \
		transferfunctionframe.h \
		rendermainwindow.h
#		navigationframe.h
MOC_SOURCES := $(subst .h,_h.cc,$(MOC_HEADERS))
MOC_OBJECTS := $(subst .cc,.o,$(MOC_SOURCES))


all: $(TARGET)
$(TARGET): $(MOC_OBJECTS) $(OBJECTS)
	$(LD) $(LDARGS) $(LIBS) -o $(TARGET) $(OBJECTS) $(MOC_OBJECTS)

gui: $(UIC_SOURCES)
	$(UIC) rendergui.ui -o rendergui.h
	$(UIC) -impl rendergui.h rendergui.ui -o rendergui.cc


clean:
	rm -f *~ *.o






