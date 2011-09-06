/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <govideocapture.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalhelper.h>
#include <gofileio.h>
#include <stdio.h>

/*
 * Example program for goVideoCapture.
 * Note that this works only for cameras providing RGB24 data,
 * see source code for modifications if you have YUV420P data.
 *
 * Christian Gosch
 */
int main()
{
    goVideoCapture vc;

    //= Set device.
    vc.setDevice ("/dev/video0");
    if (vc.open())
    {
        printf ("Opened the device /dev/video1\n");
        goSize_t w = 640;
        goSize_t h = 480;
        goSignal3D<void> sig;
        sig.setDataType (GO_UINT8);
        //= Make a signal that is linear in memory (necessary for goVideoCapture to directly grab 
        //= RGB24 data to the signal).
        sig.make (goSize3D(w,h,1),goSize3D(w,h,1),goSize3D(0,0,0),3);
        //= Get settings from device.
        vc.getSettings();
        vc.setCaptureSize (w,h);
        //= Push settings to device.
        vc.setSettings();
        goString filebase = "grabbed-";
        goString fileext = ".jpg";
        goString num;
        num.resize(3);
        //= Give the camera some time to adjust.
        sleep(5);
        //= Take 10 pictures and save them (this needs libdevil support to be enabled in golib).
        for (goIndex_t j = 0; j < 50; ++j)
        {
            //= NOTE: This assumes the camera is providing data as RGB24.
            //= If your camera happens to return YUV420P (planar) data, you can use
            //= the function goYUV420P_RGB() in gocolourspace.h to convert to RGB. If your camera
            //= supports a different format, please write a conversion routine if you can!
            vc.grab(sig);

            //= Swap R and B channels (seem to come in different byte order from 
            //= our logitech camera).
            #if 0
            {
                goSubSignal3D<void> sub0 (&sig,sig.getSizeX(),sig.getSizeY(),sig.getSizeZ());
                goSignal3D<void> temp;
                temp.setDataType(sig.getDataType().getID());
                temp.make(sig.getSize(),sig.getBlockSize(),sig.getBorderSize(),1);

                sig.setChannel(0);
                goCopySignalChannel(&sig,&temp);
                sub0.setChannel(2);
                goCopySignalChannel(&sub0,&sig);
                goCopySignalChannel(&temp,&sub0);
            }
            #endif
            goString name = filebase;
            sprintf(num.getPtr(),"%3d",j);
            name += num;
            name += fileext;
            goFileIO::writeImage(name.toCharPtr(),&sig);
        }
        vc.close();
    }
    else
    {
        printf ("Could not open device /dev/video0\n");
    }
}
