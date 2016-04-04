#Makefile automatically created by mkmakefile (C)Christian Gosch
#Template version 
# Fill in __TARGETNAME and __SOURCDIRS as well as all other options
# you might want to have added.
GTKMM_CFLAGS = `/usr/local/bin/pkg-config gtkmm-2.4 --cflags`
GTKMM_LDFLAGS = `/usr/local/bin/pkg-config gtkmm-2.4 --libs`

CXX		= g++
CXXARGS		= -c   -D HAVE_CONFIG_H -g $(GTKMM_CFLAGS) `/usr/local/bin/pkg-config --cflags golib` --std=c++11

LD		= g++
LDARGS		= $(GTKMM_LDFLAGS) $(LDFLAGS)

#GOPATH		= /home/christian/pgm/golib

LIBS = `/usr/local/bin/pkg-config --libs golib`

INCLUDE		=

TARGET		= gui

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

