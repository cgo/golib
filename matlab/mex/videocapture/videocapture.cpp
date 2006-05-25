#include "mex.h"
#include <govideocapture.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: [image] = videocapture (file_descriptor)"); \
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
    if (nlhs != 1)
    {
        USAGE();
    }

    int fd  = (int)mxGetScalar(prhs[0]);
    goVideoCapture vc;
    vc.setFileDescriptor (fd);
    int dims[3];
    dims[0] = vc.getCaptureHeight();
    dims[1] = vc.getCaptureWidth();
    dims[2] = 3;
    plhs[0] = mxCreateNumericArray(3, dims, mxUINT8_CLASS, mxREAL);
    if (!plhs[0])
    {
        mexErrMsgTxt ("Could not allocate image.");
    }
    unsigned char* buffer = (unsigned char*)malloc (sizeof(unsigned char) * dims[0] * dims[1] * dims[2]);
    if (!vc.grab (buffer, dims[0] * dims[1] * dims[2]))
    {
        mexErrMsgTxt ("Could not grab image (vc.grab() failed).");
    }
    //= Copy to matlab (I HATE Matlab and it's "everything must be a bloody matrix in bloody
    //= mathematical storage" philosophy. Costs some time in copying here :-((( ).
    unsigned char* mp = (unsigned char*)mxGetPr(plhs[0]);
    unsigned char* bp = buffer;
    int x;
    int y;
    int z;
    int yJump = dims[1] * dims[2];
    for (z = 0; z < dims[2]; ++z)
    {
        bp = buffer + z;
        unsigned char* bpy = bp;
        unsigned char* mpy = mp;
        for (y = 0; y < dims[0]; ++y)
        {
            unsigned char* bpx = bpy;
            unsigned char* mpx = mpy;
            for (x = 0; x < dims[1]; ++x)
            {
                *mpx = *bpx;
                bpx += dims[2];
                mpx += dims[0];
            }
            ++mpy;
            bpy += yJump;
        }
        mp += dims[0] * dims[1];
    }
    free(buffer);
    buffer = NULL;
}
