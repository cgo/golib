#include "mex.h"
#include <golist.h>
#include <gopoint.h>
#include <gocurve.h>
#include <govector.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: tf = turningFunction (points);tf: Turning function\npoints: original points");\
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
    if (nrhs != 1)
    {
        USAGE();
    }

    const mxArray* pointsArray = prhs[0];
    int pointsRows    = mxGetM (pointsArray);
    int pointsColumns = mxGetN (pointsArray);
    int nPoints = pointsColumns;
    if (pointsRows != 2)
    {
        mexErrMsgTxt ("Points (first argument) must be 2xN matrix.");
    }
    double* points = mxGetPr (pointsArray);
    plhs[0] = mxCreateDoubleMatrix (1, nPoints-1, mxREAL);

    goList<goPointd> pointList;
    int i;
    for (i = 0; i < nPoints; ++i)
    {
        pointList.append(goPointd(points[2*i],points[2*i+1]));
    }
    goCurve<goPointd> curve;
    curve.setPoints(pointList);
    goVectord tf;
    curve.getTurningFunction(tf);
    double* tf_p = mxGetPr(plhs[0]);
    for (i = 0; i < nPoints-1; ++i)
    {
        *tf_p = tf[i];
        ++tf_p;
    }
}
