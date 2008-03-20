import array
#s = golib.goString()
#golib.goSignalInfoText (the_image, s)
#print (s.toCharPtr())

mask = golib.goVectorf ()
mask.setArray ([1,4,6,4,1])
gaussfilter = golib.goFilter1D(mask, 1, True)
mask.setArray ([-1,0,1])
highfilter = golib.goFilter1D(mask, 1, True)
logfilter = golib.goFilter3Dv ()
m = golib.goVectorf ()
m.setArray([-1, -1, -1,
           -1, 8, -1,
            -1, -1, -1])
logfilter.setMask (m.getPtr(), 3, 3, 1, True)
logfilter.setMaskCenter (1,1,0)

fimage = golib.goSignal3Dv ()
fimage.setDataType (golib.GO_FLOAT)
fimage.make (the_image.getSize(), the_image.getBlockSize(), the_image.getBorderSize(),1)
golib.goRGBAtoScalar (the_image, fimage)

for i in xrange(2):
    gaussfilter.filter (fimage)
    fimage.rotateAxes()
    fimage.rotateAxes()
    gaussfilter.filter (fimage)
    fimage.rotateAxes()
#    highfilter.filter (fimage)
fimage2 = golib.goSignal3Dv ()
fimage2.setDataType (fimage.getDataType().getID())
fimage2.make (fimage.getSize(), fimage.getBlockSize(), fimage.getBorderSize(), 1)
logfilter.filter (fimage, fimage2)
fimage = fimage2

fimage.setChannel (0)
for i in xrange (the_image.getChannelCount()):
    the_image.setChannel (i)
    golib.goCopySignalChannel (fimage, the_image)
the_image.setChannel (0)
