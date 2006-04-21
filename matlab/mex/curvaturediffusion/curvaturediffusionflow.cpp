#include "mex.h"
#include <gonurbs.h>
#include <golist.h>
#include <gopoint.h>
#include <gocurvaturediffusion.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Blah.\n");\
}

/* --------------------------------------------------------------------------
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
    if (nrhs != 2)
    {
        USAGE();
    }

    const mxArray* pointsArray = prhs[0];
    int sigma         = (int)mxGetScalar(prhs[1]);
    int pointsRows    = mxGetM (pointsArray);
    int pointsColumns = mxGetN (pointsArray);
    if (pointsRows != 2)
    {
        mexErrMsgTxt ("Points (first argument) must be 2xN matrix.");
    }
    double* points = mxGetPr (pointsArray);
    int nPoints = pointsColumns;
    plhs[0] = mxCreateDoubleMatrix (2, nPoints, mxREAL);

    goList<goPointd> pointList;
    int i;
    for (i = 0; i < pointsColumns; ++i)
    {
        pointList.append(goPointd(points[2*i],points[2*i+1]));
    }
    pointList.close();

    goFixedArray<goPointd> f_normal (nPoints);
    goCurvatureDiffusionFlow<goPointd> (pointList, sigma, f_normal);
    
    double* result = mxGetPr(plhs[0]);
    for (i = 0; i < nPoints; ++i)
    {
        result[2*i] = f_normal[i].x;
        result[2*i+1] = f_normal[i].y;
    }
}
