# Generated automatically from Makefile.in by configure.
# Target name and version
TARGET  = Go
VERSION = 0.2.1
STATIC_TARGET := lib$(TARGET).a

# remove USE_QT if you dont want to use Qt. Qt is not entirely free.
#DEFINES		= -DUSE_QT
DEFINES		= -DHAVE_CONFIG_H -D_REENTRANT 
CC 		= c++
CCARGS 		= -c $(DEFINES) -O2 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -Wall -Wno-non-template-friend 
# add this for gcc 2.95
#-Wno-non-template-friend
CCLIBARGS 	= 
LD 		= c++ 
NASM		= nasm
LDARGS 		= -mt 
LDLIBARGS	= -shared -Wl,-soname,lib$(TARGET).so.$(VERSION)
#MOC		= @MOC@
DOCXX		= echo
DOXYGEN		= doxygen #doxygen.config
DOXYGEN_CONFIG = doxygen.config

# Static options
# AR is set by configure

# Project root
ROOT = .

#Documentation directory
DOC  = $(ROOT)/doc
#Latex documentation file
DOC_LATEX = golib.tex

# Install directory
INSTALLDIR = $(ROOT)/lib
SRCDIR	   = $(ROOT)/src

# Libraries; remove qt if you dont want to use it.
LIBS = -lSDL -lnsl -ltiff -lpthread   -lSM -lICE  -L/usr/X11R6/lib 
#LIBS = -L/usr/X11R6/lib -lX11 -lstdc++ -lm -lqt

# Includes
INCLUDE = -I$(ROOT)/include  -I/usr/X11R6/include


# Rules
.cc.o:
	$(CC) $(CCARGS) $(INCLUDE) $(CCLIBARGS) $< -o $@
#.h.moc:
#	$(MOC) $< -o $@
#.moc.o:
#	$(CC) $(CCARGS) $(INCLUDE) $< -o $@
#.h.o:
#	$(MOC) $< -o tmp_moc.cc
#	$(CC) $(CCARGS) $(INCLUDE) tmp_moc.cc -o $@

.s.o:
	$(NASM) -f elf $< -o $@

# Files
#MOC_DIRS := $(ROOT)/include/non-free/qtwidgets $(ROOT)/include/non-free/depot
#MOC_H    := $(foreach DIR,$(MOC_DIRS),$(wildcard $(DIR)/*.h))
#MOC_RT    = $(ROOT)/include/non-free
#MOC_H    := $(MOC_RT)/depot/gopaperframe.h $(MOC_RT)/depot/godepotframe.h $(MOC_RT)/qtwidgets/goinputwidget.h $(MOC_RT)/qtwidgets/goqtdisplay.h
#MOC_OBJS := $(subst .h,.o,$(MOC_H))

# Dependencies
ALL: $(TARGET)

# Files to be moc'ed. Used ONLY by Qt.
#$(MOC_RT)/qtwidgets/goinputwidget.o: 	$(MOC_RT)/qtwidgets/goinputwidget.h
#$(MOC_RT)/qtwidgets/goqtdisplay.o: 	$(MOC_RT)/qtwidgets/goqtdisplay.h
#$(MOC_RT)/depot/gopaperframe.o: 	$(MOC_RT)/depot/gopaperframe.h
#$(MOC_RT)/depot/godepotframe.o: 	$(MOC_RT)/depot/godepotframe.h

#Documentation files for DOC++
DOC_DIRS  := . data nifty misc math signal graphics display network thread system
DOC_FILES := $(foreach DIR,$(DOC_DIRS),$(wildcard $(ROOT)/include/$(DIR)/*.h))

# Objects
DIRS := data nifty misc math signal graphics display network non-free/qtwidgets non-free/depot thread system mmx
SRC := $(foreach DIR,$(DIRS),$(wildcard $(SRCDIR)/$(DIR)/*.cc))
OBJS := $(subst .cc,.o,$(SRC))
ASM_SRC := $(foreach DIR,$(DIRS),$(wildcard $(SRCDIR)/$(DIR)/*.s))
ASM_OBJS := $(subst .s,.o,$(ASM_SRC))

# Files to be removed when make clean is called.
# All object files are removed by default.
REMOVE := $(foreach DIR,$(DIRS),$(wildcard $(SRCDIR)/$(DIR)/*~)) $(ROOT)/*~
REMOVE_H := $(foreach DIR,$(DIRS),$(wildcard $(ROOT)/include/$(DIR)/*~)) $(wildcard $(ROOT)/include/defs/*~)


$(TARGET): $(OBJS) $(ASM_OBJS)
# remove MOC_OBJS if you dont want to use Qt.
#	$(LD) $(LIBS) $(LDARGS) $(LDLIBARGS) $(MOC_OBJS) $(OBJS) $(ASM_OBJS) -o lib$(TARGET).so.$(VERSION)
	$(LD) $(LIBS) $(LDARGS) $(LDLIBARGS) $(OBJS) $(ASM_OBJS) -o lib$(TARGET).so.$(VERSION)
	if test -d $(INSTALLDIR); then echo "$(INSTALLDIR) exists"; else mkdir $(INSTALLDIR); fi
	mv lib$(TARGET).so.$(VERSION) $(INSTALLDIR)
	rm -f $(INSTALLDIR)/lib$(TARGET).so
	ln -s lib$(TARGET).so.$(VERSION) $(INSTALLDIR)/lib$(TARGET).so

doc:	$(DOC_FILES)
	$(DOCXX) -f -d $(DOC)/html $(DOC_FILES)
	$(DOCXX) -f -t -o $(DOC)/latex/$(DOC_LATEX) $(DOC_FILES)
	$(DOXYGEN) $(DOXYGEN_CONFIG)

doc-vol: $(DOC_FILES)
	$(DOXYGEN) doxygen-vol.config

static: $(OBJS)
ifeq ($(AR),NONE)
else
	$(AR) -rs $(STATIC_TARGET) $(OBJS) $(ASM_OBJS)
	ranlib $(STATIC_TARGET)
	chmod +x $(STATIC_TARGET)
	mv $(STATIC_TARGET) $(INSTALLDIR)
endif

clean:	
	rm -rf $(OBJS)
	rm -rf $(ASM_OBJS)
	rm -rf $(REMOVE)
	rm -rf $(REMOVE_H)


stat:
	@echo
	@echo Lines:
	wc -l $(SRC)
	@echo
	@echo Bytes:
	wc -c $(SRC)









