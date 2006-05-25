#include <govideocapture.h>
#include <gosignal3d.h>
#include <gofileio.h>
#include <stdio.h>

int main()
{
    goVideoCapture vc;

    vc.setDevice ("/dev/video0");
    if (vc.open())
    {
        printf ("YES!\n");
        goSignal3D<void> sig;
        sig.setDataType (GO_UINT8);
        goSize_t w = 640;
        goSize_t h = 480;
        vc.setCaptureSize (w,h);
        vc.setColourMode (goVideoCapture::RGB);
        sig.make (goSize3D(w,h,1),goSize3D(w,h,1),goSize3D(0,0,0),3);
        //= Give the camera a little time to adjust ...
        // sleep(4);
        vc.initDevice();
        goString filebase = "grabbed-";
        goString fileext = ".jpg";
        goString num;
        num.resize(3);
        for (goIndex_t j = 0; j < 100; ++j)
        {
            vc.grab(sig);
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
        printf ("NO!\n");
    }
}
