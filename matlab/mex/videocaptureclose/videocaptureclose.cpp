/**
 * @addtogroup matlab
 * @{
 * @file videocaptureclose.cpp
 * This mex module provides a function to close a video4linux
 * device.
 * \par Usage
 * In matlab, call <code> videocaptureclose (fd); </code>
 * @}
 */

#include "mex.h"
#include <govideocapture.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: videocaptureclose (file_descriptor)"); \
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
    if (nrhs != 1)
    {
        USAGE();
    }
    else
    {
        printf ("Matlab V4L interface\nUsing golib 0.5 with libGomatlab\nCopyright Christian Gosch. All rights reserved.\n");
    }

    int fd  = (int)mxGetScalar(prhs[0]);
    goVideoCapture vc;
    vc.setFileDescriptor (fd);
    vc.close();
}
