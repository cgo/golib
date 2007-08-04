from golib import *
import math

x = goVectorf()
y = goVectorf()
M = goMatrixf(100,2)
M.refColumn(0,x)
M.refColumn(1,y)
x.fillRange(0.0,math.pi/50.0,2.0*math.pi)
x.copy(y)
goSinf(y)
M.writeASCII("sin.txt")
x.copy(y)
goCosf(y)
M.writeASCII("cos.txt")

p = Plot ()
p.plot (M)
#p.plot (M, "", "w l", 0, 1)
p.plot ()

gp = goGnuplot()
mp = goMultiPlotter(1,1)
plot = goSinglePlot()
plot.addCurveMatrix(M, "cos")
mp.addPlot(plot,0)
mp.plot(gp)
