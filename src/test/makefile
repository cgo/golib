CC = gcc -c -g
LD = gcc -g
INCLUDE = -I../../include -I$(QTDIR)/include
#LIBS    = -L../../lib -lqt -lGo
LIBS    = -L../../lib -lGo -lstdc++ -lqt -ljpeg -lz

.cc.o:
	$(CC) $(INCLUDE) $< -o $@

%: %.cc
	rm -f $@.o $@
	$(CC) $(INCLUDE) $< -o $@.o
	$(LD) $(LIBS) $@.o -o $@

ALL: testmath
test: test.o
	gcc -g -o test test.o -L../../lib -L$(QTDIR)/lib -lqt -lGo $(LIBS)
test.o: test.cc
	gcc -g -c -o test.o test.cc -I../../include -I$(QTDIR)/include
testmath.o: testmath.cc
testmath: testmath.o
	$(LD) $(LIBS) $< -o $@ -L$(QTDIR)/lib -lqt

hash.o: hash.cc
hash: hash.o
	$(LD) $(LIBS) $< -o $@ -L$(QTDIR)/lib -lqt

44.o: 44.cc
44: 44.o
	$(LD) $(LIBS) $< -o $@ -L$(QTDIR)/lib -lqt

x11.o: x11.cc

x11: x11.o
	$(LD) $(LIBS) $< -o $@

configfile.o: configfile.cc

configfile: configfile.o
	$(LD) $(LIBS) $< -o $@

config: config.o
	$(LD) $(LIBS) $< -o $@

config.o: config.cc

complex: complex.o
	$(LD) $(LIBS) $< -o $@

integral: integral.o
	$(LD) $(LIBS) $< -o $@

signal: signal.o
	$(LD) $(LIBS) $< -o $@

connect: connect.o 
	$(LD) $(LIBS) $< -o $@
connect2: connect2.o 
	$(LD) $(LIBS) $< -o $@

tree: tree.o 
	$(LD) $(LIBS) $< -o $@

qtdisplay: qtdisplay.o
	$(LD) $(LIBS) $< -o $@
clean:
	rm -f test.o test testmath.o testmath *~










