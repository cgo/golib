#Makefile automatically created by mkmakefile (C)Christian Gosch

CXX		= gcc
CXXARGS		= -c -D_REENTRANT  -D HAVE_CONFIG_H 

LD		= gcc
LDARGS		=  

LIBS		=   -L$(GOPATH)/lib -lGo -lm \
			 -L/usr/X11R6/lib -lSM -lICE -lX11 -lXi -lXmu \
			-lGL -lglut -lGLU 

INCLUDE		=  -I. -I$(GOPATH)/include

TARGET		= testpos

%.o: %.cxx
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
%.o: %.cpp
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
%.o: %.cc
	$(CXX) $(CXXARGS) $(INCLUDE) -o $@ $<
SOURCEDIRS = .
SOURCES = goposdisplay3d.cc testpos.cc
#SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cxx))
#SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cpp))
#SOURCES := $(SOURCES) $(foreach DIR,$(SOURCEDIRS),$(wildcard $(DIR)/*.cc))
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
