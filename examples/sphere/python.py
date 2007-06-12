import math

cartesian = goMatrixf()
sampleViewSphere (0.5, 3.0, cartesian)
x,y,z = goVectorf(),goVectorf(),goVectorf()
cartesian.refColumn(0,x)
cartesian.refColumn(1,y)
cartesian.refColumn(2,z)

plotter = goMultiPlotter(1,1)
plot = goSinglePlot()
plot.add3D (x,y,1,z,"","with points")
plotter.setPauseFlag(True)
plotter.addPlot(plot,0)
plotter.plot()

cartesian2 = goMatrixf(cartesian.getRows(),cartesian.getColumns())
for i in xrange(cartesian.getRows()):
    v,v2 = goVectorf(),goVectorf()
    cartesian.refRow(i,v)
    phi,theta,r = euclideanToSphere (v)
    print phi/math.pi*180.0,theta/math.pi*180.0,r
    cartesian2.refRow(i,v2)
    temp = goVectorf()
    sphereToEuclidean (phi,theta,r,v2,temp)

print "This should be a matrix of zeroes:"
print cartesian - cartesian2
