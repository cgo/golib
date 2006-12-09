import golib
import gomatlab
from gomatlab import *
from golib import *
import sys

dwt = goDWT3D()
image_d = goSignal3Dv()
goFileIO.readImage (sys.argv[1], image_d)

image = goSignal3Dv()
image.setDataType (GO_FLOAT)
image.make (image_d.getSizeX(), image_d.getSizeY(), image_d.getSizeZ(), 32, 32, 1, 32, 32, 0, 1)
goCopySignal (image_d, image)

frameMode = True

dwt.setFrameMode(frameMode)
dwt.setFilter(goDWT3D.HAAR, 0)
dwt.dwt(image, 1, GO_FLOAT)

matlab = goMatlab()
matlab.putSignal (dwt.getDWT().get(0).__deref__(), "ll")
matlab.putSignal (dwt.getDWT().get(1).__deref__(), "lh")
matlab.putSignal (dwt.getDWT().get(2).__deref__(), "hl")
matlab.putSignal (dwt.getDWT().get(3).__deref__(), "hh")

matlab.matlabCall ("subplot(2,2,1), imagesc(ll)")
matlab.matlabCall ("subplot(2,2,2), imagesc(lh)")
matlab.matlabCall ("subplot(2,2,3), imagesc(hl)")
matlab.matlabCall ("subplot(2,2,4), imagesc(hh)")

recon = goSignal3Dv()

dwt.idwt(recon)

D = goSignal3Dv()
D.copy(recon)

D -= image
D *= D

matlab.putSignal (recon, "recons")
matlab.putSignal (D, "D")
s = goString()
s.resize(1024)
matlab.matlabCall ("figure(), imagesc (recons), title('recons'), sum(sum(D))", s)
if not (s.toCharPtr() == ""):
    print s.toCharPtr()

recon2 = goSignal3Dv()
recon2.setDataType(GO_UINT8)
recon2.make(recon.getSize(), recon.getBlockSize(), recon.getBorderSize(), 1)
goCopySignal(recon, recon2)
goFileIO.writeImage("recon.jpg", recon2)


dwt2 = goDWT3D()
dwt2.setFrameMode(frameMode)
dwt2.setFilter(goDWT3D.HAAR, 1)
dwt2.dwt(dwt.getDWT().get(0).__deref__(), 1, GO_FLOAT)

matlab.putSignal (dwt2.getDWT().get(0).__deref__(), "ll2")
matlab.putSignal (dwt2.getDWT().get(1).__deref__(), "lh2")
matlab.putSignal (dwt2.getDWT().get(2).__deref__(), "hl2")
matlab.putSignal (dwt2.getDWT().get(3).__deref__(), "hh2")

matlab.matlabCall ("figure(), subplot(2,2,1), imagesc(ll2)")
matlab.matlabCall ("subplot(2,2,2), imagesc(lh2)")
matlab.matlabCall ("subplot(2,2,3), imagesc(hl2)")
matlab.matlabCall ("subplot(2,2,4), imagesc(hh2)")

dwt3 = goDWT3D()
dwt3.setFrameMode(frameMode)
dwt3.setFilter(goDWT3D.HAAR, 2)
dwt3.dwt(dwt2.getDWT().get(0).__deref__(), 1, GO_FLOAT)

matlab.putSignal (dwt3.getDWT().get(0).__deref__(), "ll3")
matlab.putSignal (dwt3.getDWT().get(1).__deref__(), "lh3")
matlab.putSignal (dwt3.getDWT().get(2).__deref__(), "hl3")
matlab.putSignal (dwt3.getDWT().get(3).__deref__(), "hh3")

matlab.matlabCall ("figure(), subplot(2,2,1), imagesc(ll3)")
matlab.matlabCall ("subplot(2,2,2), imagesc(lh3)")
matlab.matlabCall ("subplot(2,2,3), imagesc(hl3)")
matlab.matlabCall ("subplot(2,2,4), imagesc(hh3)")
