#Makefile automatically created by mkmakefile (C)Christian Gosch
#Template version 
# Fill in __TARGETNAME and __SOURCDIRS as well as all other options
# you might want to have added.
GTKMM_CFLAGS = `pkg-config gtkmm-3.0 --cflags`
GTKMM_LDFLAGS = `pkg-config gtkmm-3.0 --libs`

CXX		= g++
CXXARGS		= -c   -D HAVE_CONFIG_H -g $(GTKMM_CFLAGS) `pkg-config --cflags golib` -std=c++11

LD		= g++
LDARGS		= $(GTKMM_LDFLAGS) $(LDFLAGS) `pkg-config --libs golib`

#GOPATH		= /home/christian/pgm/golib

LIBS =   #-lGomatlab 

INCLUDE		=  

TARGET		= imageviewer

%.o: %.cxx
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
%.o: %.cpp
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
%.o: %.cc
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
SOURCEDIRS = .
SOURCES = 
SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cxx))
SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cpp))
SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cc))
OBJECTS0 = $(SOURCES)
OBJECTS0 := $(subst .cxx,.o,$(OBJECTS0))
OBJECTS1 := $(subst .cpp,.o,$(OBJECTS0))
OBJECTS2 := $(subst .cc,.o,$(OBJECTS1))
OBJECTS = $(OBJECTS2)
all: $(TARGET)
$(TARGET): $(OBJECTS)
	$(LD) $(LDARGS) $(LIBS) -o $(TARGET) $(OBJECTS)
clean:
	rm -f *~ *.o

