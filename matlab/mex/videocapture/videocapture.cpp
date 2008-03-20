/**
 * @addtogroup matlab
 * @{
 * @file videocapture.cpp
 * This mex module provides a simple video capture function.
 * \par Usage
 * In matlab, call <code> image = videocapture (fd); </code> where \c fd is 
 * a file descriptor from the videocaptureopen mex module.
 * @}
 */

#include "mex.h"
#include <govideocapture.h>
#include <gocolourspace.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: [image] = videocapture (file_descriptor)\nCopyright Christian Gosch, all rights reserved."); \
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
    dims[2] = 1;
    if (vc.getColourMode() == goVideoCapture::RGB24 ||
        vc.getColourMode() == goVideoCapture::YUV422P ||
        vc.getColourMode() == goVideoCapture::YUV420P)
    {
        dims[2] = 3;
    }
    int dimsMatlab[3];
    dimsMatlab[0] = dims[0];
    dimsMatlab[1] = dims[1];
    dimsMatlab[2] = 3;
    plhs[0] = mxCreateNumericArray(3, dimsMatlab, mxUINT8_CLASS, mxREAL);
    if (!plhs[0])
    {
        mexErrMsgTxt ("Could not allocate image.");
    }
    unsigned char* buffer = (unsigned char*)malloc (sizeof(unsigned char) * dims[0] * dims[1] * dims[2]);
    if (!vc.grab (buffer, dims[0] * dims[1] * dims[2]))
    {
        free(buffer);
        mexErrMsgTxt ("Could not grab image (vc.grab() failed).");
    }
    //= Copy to matlab (I HATE Matlab and its "everything must be a bloody matrix in bloody
    //= mathematical storage" philosophy. Costs some time in copying here :-((( ).
    switch(vc.getColourMode())
    {
        case goVideoCapture::RGB24:
            {
                unsigned char* mp = (unsigned char*)mxGetPr(plhs[0]);
                unsigned char* bp = buffer;
                int x;
                int y;
                int z;
                int yJump = dims[1] * dims[2];
                for (z = dims[2]-1; z >= 0; --z)
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
            } break;
        case goVideoCapture::YUV420P:
        case goVideoCapture::YUV422P:
            {
                goSignal3D<void> rgb_buffer;
                rgb_buffer.setDataType(GO_UINT8);
                rgb_buffer.make (goSize3D(dims[1],dims[0],1),goSize3D(dims[1],dims[0],1),goSize3D(2,2,0),3);
                goYUV420P_RGB (buffer, dims[1], dims[0], rgb_buffer);
                unsigned char* mp = (unsigned char*)mxGetPr(plhs[0]);
                unsigned char* buffer2 = (unsigned char*)rgb_buffer.getPtr();
                int x;
                int y;
                int z;
                int yJump = dims[1] * dims[2];
                for (z = 0; z < dims[2]; ++z)
                {
                    unsigned char* bp = buffer2 + z;
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
            } break;
#if 0
            {
                unsigned char* mp = (unsigned char*)mxGetPr(plhs[0]);
                unsigned char* bp = buffer;
                int x;
                int y;
                int z;
                int yJump = dims[1]; // * dims[2];
                int d = 0;
                for (z = 0; z < dims[2]; ++z)
                {
                    bp = buffer;
                    unsigned char* bpy = bp;
                    unsigned char* mpy = mp;
                    for (y = 0; y < dims[0]; ++y)
                    {
                        unsigned char* bpx = bpy;
                        unsigned char* mpx = mpy;
                        for (x = 0; x < dims[1]; ++x)
                        {
                            *mpx = *bpx;
#if 0
                            if (d == 0)
                            {
                                *mpx = static_cast<unsigned char>(goYUV422_Red(*bpx, *(bpx+1), *(bpx+2)) * 255.0);
                            }
                            else
                            {
                                if (d == 1)
                                {
                                    // *mpx = static_cast<unsigned char>(goYUV422_Green(*bpx, *(bpx+1), *(bpx+2)) * 255.0);
                                }
                                else
                                {
                                    if (d == 2)
                                    {
                                        // *mpx = static_cast<unsigned char>(goYUV422_Blue(*bpx, *(bpx+1), *(bpx+2)) * 255.0);
                                    }
                                }
                            }
#endif
                            bpx += 1; // dims[2];
                            mpx += dims[0];
                        }
                        ++mpy;
                        bpy += yJump;
                    }
                    mp += dims[0] * dims[1];
                    ++d;
                }
            } break;
#endif
        default:
            {
                free(buffer);
                mexErrMsgTxt("Unsupported colour mode.");
            } break; 
    }
    free(buffer);
    buffer = NULL;
}
