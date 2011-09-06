/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include "mex.h"
#include <golist.h>
#include <gopoint.h>
#include <gocurve.h>
#include <godiscretecurvepartition.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: newpoints = curvesimplify(curve_points, max_segment_length);curve_points: Points on the curve\nnewpoints: New points\nmax_segment__length: Maximal segment length (absolute)");\
}

/* --------------------------------------------------------------------------
* Simplifies a curve consisting of digital segments.
* See golib source code for details. Uses Latecki/Lakaemper's curve
* evolution method.
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
    int pointsRows    = mxGetM (pointsArray);
    int pointsColumns = mxGetN (pointsArray);
    int nPoints = pointsColumns;
    if (pointsRows != 2)
    {
        mexErrMsgTxt ("Points (first argument) must be 2xN matrix.");
    }
    double* points = mxGetPr (pointsArray);
    double maxSegmentLength = (double)mxGetScalar(prhs[1]);
    
    goList<goPointd> pointList;
    int i;
    for (i = 0; i < nPoints; ++i)
    {
        pointList.append(goPointd(points[2*i],points[2*i+1]));
    }
    goCurve<goPointd> curve;
    curve.setPoints(pointList);
    goDiscreteCurvePartition<goDouble> dcp;
    dcp.setCurve(&curve);
    dcp.lateckiSimplify(3,maxSegmentLength);

    goList<goPointd>* newPoints = &curve.getPoints();
    if (!newPoints)
    {
        mexErrMsgTxt ("curvesimplify(): newPoints == NULL!\n");
    }
    nPoints = newPoints->getSize();
    plhs[0] = mxCreateDoubleMatrix (2, nPoints, mxREAL);
    points = mxGetPr(plhs[0]);
    goList<goPointd>::Element* el = newPoints->getFrontElement();
    i = 0;
    while (el && i < nPoints)
    {
        *points = el->elem.x;
        ++points;
        *points = el->elem.y;
        ++points;
        ++i;
        el = el->next;
    }
}
