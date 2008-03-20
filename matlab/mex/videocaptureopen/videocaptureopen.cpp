/**
 * @addtogroup matlab
 * @{
 * @file videocaptureopen.cpp
 * This mex module provides a function to open a video4linux
 * device.
 * \par Usage
 * In matlab, call <code> fd = videocaptureopen (device_filename, width, height); </code>
 * @}
 */

#include "mex.h"
#include <govideocapture.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: [file_descriptor] = videocaptureopen (devicefile_string, width, height)"); \
}

/* --------------------------------------------------------------------------
* @brief newpoints = resample2d(points,num);
* Resamples a 2D curve given by 2xN-array points equidistantly
* and returns num equidistant points on the curve.
----------------------------------------------------------------------------*/
void mexFunction (int            nlhs, 
                  mxArray*       plhs[],
                  int            nrhs,
                  const mxArray* prhs[])
{
    if (nlhs != 1)
    {
        USAGE();
    }
    if (nrhs != 3)
    {
        USAGE();
    }

    char deviceName[1024];
    if (mxGetString (prhs[0], deviceName, 1024) != 0)
    {
        USAGE();
    }
    int width = 0;
    int height = 0;
    width  = (int)mxGetScalar(prhs[1]);
    height = (int)mxGetScalar(prhs[2]);
    goVideoCapture vc;
    vc.setDevice (deviceName);
    if (!vc.open())
    {
        plhs[0] = mxCreateDoubleScalar (-1.0);
        mexErrMsgTxt ("Could not open device.");
    }
    else
    {
        printf ("Matlab V4L interface\nUsing golib 0.5 with libGomatlab\nCopyright Christian Gosch. All rights reserved.\n");
    }
    vc.getSettings();  // get settings from device instead of trying to initialise it.
    vc.setCaptureSize (width,height);
    // vc.setColourMode (goVideoCapture::RGB24);
    if (!vc.setSettings())
    {
        plhs[0] = mxCreateDoubleScalar (-1.0);
        mexErrMsgTxt ("Could not set settings to device.");
    }
    plhs[0] = mxCreateDoubleScalar ((double)vc.getFileDescriptor());
}
