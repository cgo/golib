import golib
from golib import *

plotter = goMultiPlotter (1,1)
plot = goSinglePlot ()

P = goMatrixf (10,2)
for i in xrange(P.getRows()):
    for j in xrange(P.getColumns()):
        P.set(i,j,goRandom())

plot.addCurveMatrix (P, "P")

print "P:"
P._print ()

P2 = goMatrixf (P)
P2.transpose()
P2 = P2 * P

P2.power (0.5)
P2.invert ()

P = P * P2
print "P * Sp:"
P._print ()
plot.addCurveMatrix (P, "P * Sp")

I = goMatrixf()
goMatrixMultf (1, P, True, P, False, 1, I)
print "Hopefully Id:"
I._print ()

plotter.addPlot (plot, 0, 0)
plotter.setPauseFlag (True)
plotter.plot ()

m = goMatrixf (10,10)
for i in xrange(m.getRows()):
    for j in xrange(m.getColumns()):
        m.set(i,j,2*i+j)
m2 = goMatrixf ()
m.ref (3, 2, 3, 4, m2)
#m2.transpose()
m._print ()
m2._print ()
