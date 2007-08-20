#Makefile automatically created by mkmakefile (C)Christian Gosch

CXX		= gcc
CXXARGS		= -c   

LD		= gcc
LDARGS		= $(LDFLAGS)

LIBS		=  -lGo

INCLUDE		=  -I$(GOPATH)/include $(CPPFLAGS)

TARGET		= integrate

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
