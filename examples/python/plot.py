import golib
import shape
import gomatlab
import sys

def test_1d():
    plotter = golib.goMultiPlotter (1,1)
    plot = golib.goSinglePlot()
    arrayx = golib.goVectord(5)
    arrayx.set(0,1)
    arrayx.set(1,2)
    arrayx.set(2,4)
    arrayx.set(3,2)
    arrayx.set(4,3)
    arrayy = golib.goVectord(5)
    arrayy.set(0,0)
    arrayy.set(1,1)
    arrayy.set(2,2)
    arrayy.set(3,3)
    arrayy.set(4,4)
    plot.addCurve (arrayx, arrayy, "Curve")
    plotter.addPlot (plot, 0, 0)
    plotter.setPauseFlag(True)
    plotter.plot()

#test_1d()

image = golib.goSignal3Dv()
golib.goFileIO.readImage ("/home/christian/Documents/images/zebras/zebra_3.jpg", image)
greyimage = golib.goSignal3Dv()
greyimage.setDataType(golib.GO_DOUBLE)
greyimage.make(image.getSize(), image.getSize(), image.getBorderSize(), 1)
golib.goRGBAtoScalar(image,greyimage)
mean = golib.goSignalMean(greyimage)
greyimage -= mean
ls = shape.goRegionLS()
ls.setPhi (greyimage)
ls.reinitialise()

mat = gomatlab.goMatlab()
mat.putSignal (ls.getPhi(),"phi")
msg = golib.goString()
msg.resize(1024)
mat.matlabCall ("contour(phi,[0 0])", msg)
print msg.toCharPtr()

print "Press return when ready."
sys.stdin.readline()

#plot = golib.goSinglePlot()
#plotter = golib.goMultiPlotter(1,1)
#plot.add3D (ls.getPhi(), "")
#plot.setTitle (golib.goString("Reinitialised image"))
#plotter.addPlot (plot, 0, 0)
#plotter.setPauseFlag (True)
#plotter.plot ()
