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
    mexErrMsgTxt ("Usage: angle = turnangle(points);\npoints: 3 consecutive points.");\
}

template <class pointT>
static goDouble getTurn (const pointT& p1, const pointT& p2, const pointT& p3);

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
    if (nrhs != 1)
    {
        USAGE();
    }

    const mxArray* pointsArray = prhs[0];
    int pointsRows    = mxGetM (pointsArray);
    int pointsColumns = mxGetN (pointsArray);
    int nPoints = pointsColumns;
    if (pointsRows != 2 || nPoints != 3)
    {
        mexErrMsgTxt ("Points (first argument) must be 2x3 matrix.");
    }
    double* points = mxGetPr (pointsArray);
    goPointd p1 (points[0],points[1]);
    goPointd p2 (points[2],points[3]);
    goPointd p3 (points[4],points[5]);
    plhs[0] = mxCreateDoubleMatrix(1,1,mxREAL);
    *mxGetPr(plhs[0]) = getTurn<goPointd> (p1,p2,p3);
}

template <class pointT>
static goDouble getTurn (const pointT& p1, const pointT& p2, const pointT& p3)
{
    pointT base = p3 - p1;
    goDouble f = base.abs();
    if (f == 0.0)
    {
        assert ("p3 == p1" == 0);
        return 0.0;  //= This should not happen. It would mean p3 == p1.
    }
    base *= 1.0 / f;
    pointT s1 = p2 - p1;
    pointT s2 = p3 - p2;
    goDouble l1 = s1.abs();
    goDouble l2 = s2.abs();
    assert (l1 != 0.0 && l2 != 0.0);

    goDouble alpha1 = (s1 * base) / l1;
    //= acos seems to result in nan when the argument is exactly 1.0 (contrary to the manpage!).
    //= Catch that.
    if (alpha1 <= -1.0)
    {
        alpha1 = M_PI;
    }
    else
    {
        if (alpha1 >= 1.0)
        {
            alpha1 = 0.0;
        }
        else
        {
            alpha1 = acos (alpha1);
        }
    }
    goDouble alpha2 = (s2 * base) / l2;
    if (alpha2 <= -1.0)
    {
        alpha2 = M_PI;
    }
    else
    {
        if (alpha2 >= 1.0)
        {
            alpha2 = 0.0;
        }
        else
        {
            alpha2 = acos (alpha2);
        }
    }

    goDouble beta = alpha1 + alpha2;

    //= The sign of the turn angle is determined by whether the triangle {p1,p2,p3} 
    //= turns clockwise or counter-clockwise, which in turn is determined
    //= by the z-component of the cross product {s1 0} x {s2 0}.
    return beta * ((s1.x * s2.y < s1.y * s2.x) ? -1.0 : 1.0);
}
