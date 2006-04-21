#include "mex.h"
#include <gonurbs.h>
#include <golist.h>
#include <gopoint.h>
#include <gocurvepartition.h>
#include <goplot.h>
#include <gostring.h>

#define USAGE()\
{\
    mexErrMsgTxt ("Usage: curvepartition(curve_points,neighSize,filterCount,ps_filename).");\
}

/* --------------------------------------------------------------------------
* @brief partitionpoints = curvepartition(curve_points,sigma,curv_epsilon);
* Calculates a mean curvature over the neighbourhood of size 2*sigma around
* each curve point.
* Mean curvature calculation as in Delingette et al., 
* "Shape and Topology Constraints on Parametric Active Contours", page 21.
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
    if (nrhs != 4)
    {
        USAGE();
    }

    const mxArray* pointsArray = prhs[0];
    int neighSize     = (int)mxGetScalar(prhs[1]);
    int filterCount   = (int)mxGetScalar(prhs[2]);
    int maxBufLen = mxGetM(prhs[3]) * mxGetN(prhs[3]) * sizeof(mxChar) + 1;
    char* ps_filename = new char[maxBufLen];
    mxGetString (prhs[3], ps_filename, maxBufLen);
    
    // double curv_epsilon = (double)mxGetScalar(prhs[2]);
    int pointsRows    = mxGetM (pointsArray);
    int pointsColumns = mxGetN (pointsArray);
    int nPoints       = pointsColumns;
    if (pointsRows != 2)
    {
        mexErrMsgTxt ("Points (first argument) must be 2xN matrix.");
    }
    double* points = mxGetPr (pointsArray);

    goList<goPointd> pointList;
    int i;
    for (i = 0; i < pointsColumns; ++i)
    {
        pointList.append(goPointd(points[2*i],points[2*i+1]));
    }
    pointList.close();
    
    goList<goPointd> retPoints;
    goList<goIndex_t> retIndex;
    // goCurvePartition (pointList,sigma,curv_epsilon,retPoints);
    // goCurvePartitionLabelFiltering (pointList,filterCount,retPoints);
    // goCurvePartitionTrigger(pointList,triggerLevelNeg,triggerLevelPos,retPoints);
    goFixedArray<goDouble> curvature;
    goFixedArray<goDouble> meanCurvature;
    goCurvePartitionLocalMeanCurvature(pointList,neighSize,filterCount,retPoints, retIndex, &curvature, &meanCurvature);
    goString postfix = "> ";
    postfix += ps_filename;
    postfix += ".ps";
    //= Calling gnuplot here causes a test program to hang when
    //= it's calling matlab from C++ code. I have not yet spotted the
    //= cause. When needed, uncomment. It seems to work from pure matlab
    //= code.
//    goPlot::gnuplot(curvature,"Curvature","with lines\n","set terminal postscript",postfix.toCharPtr());
    postfix = "> ";
    postfix += ps_filename;
    postfix += "_mean.ps";
//    goPlot::gnuplot(meanCurvature,"Locally mean curvature","with lines\n","set terminal postscript",postfix.toCharPtr());
    plhs[0] = mxCreateDoubleMatrix (2, retPoints.getSize(), mxREAL);
    double* ret = mxGetPr(plhs[0]);
    goList<goPointd>::Element* el = retPoints.getFrontElement();
    i = 0;
    while (el && i < retPoints.getSize())
    {
        ret[2*i] = el->elem.x;
        ret[2*i+1] = el->elem.y;
        el = el->next;
        ++i;
    }
}

