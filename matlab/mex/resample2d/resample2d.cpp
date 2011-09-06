/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include "mex.h"
#include <gonurbs.h>
#include <golist.h>
#include <gopoint.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: newpoints = resample2d (points,num);\nnewpoints: resampled, uniformly spaced points\npoints: original points\nnum: number of points to sample");\
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
    if (nrhs != 2)
    {
        USAGE();
    }

    const mxArray* pointsArray = prhs[0];
    int resPoints     = (int)mxGetScalar(prhs[1]);
    int pointsRows    = mxGetM (pointsArray);
    int pointsColumns = mxGetN (pointsArray);
    if (pointsRows != 2)
    {
        mexErrMsgTxt ("Points (first argument) must be 2xN matrix.");
    }
    double* points = mxGetPr (pointsArray);
    plhs[0] = mxCreateDoubleMatrix (2, resPoints, mxREAL);

    goList<goPointd> pointList;
    int i;
    for (i = 0; i < pointsColumns; ++i)
    {
        pointList.append(goPointd(points[2*i],points[2*i+1]));
    }
    pointList.close();
    goNURBS nurbs;
    nurbs.interpolate(pointList);
    goDouble l = nurbs.getCurveLength();
    goDouble step = l / (float)(resPoints-1);
    goDouble pos = 0.0;
    goPointf p;
    double* result = mxGetPr(plhs[0]);
    for (i = 0; i < resPoints; ++i)
    {
        p = nurbs(pos);
        pos += step;
        result[2*i] = p.x;
        result[2*i+1] = p.y;
    }
}
