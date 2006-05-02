/*
 * This file contains some experiments on
 * curve partitioning.
 * Curves are here always closed 2D-curves.
 * The points are stored in closed goList structures
 * and are usually of type goPointd.
 */

#include <gocurvepartition.h>
#include <golist.h>
#include <gopoint.h>
#include <golog.h>
#include <gosignal3d.h>
#include <gosignalhelper.h>
#include <gofilter1d.h>
#include <goplot.h>
#include <gofixedarray.h>
#include <gocurvaturediffusion.h>
#include <gocurvature.h>

template <class pointT>
static bool findInflexionPoints (const goFixedArray<goDouble>& curvature, 
                                 const goList<pointT>& curvePoints, 
                                 goList<pointT>& ret, 
                                 goList<goIndex_t>& indexRet)
{
    //= Find possible inflection points.
    typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
    goIndex_t i = 0;
    goIndex_t nPoints = curvePoints.getSize();
    while (el && i < nPoints - 1)
    {
        if (curvature[i] * curvature[i+1] < 0.0f)
        {
            //ret.append((el->elem + el->next->elem) * 0.5);
            ret.append(el->elem);
            indexRet.append(i);
        }
        el = el->next;
        ++i;
    }
    if (curvature[nPoints-1] * curvature[0] < 0.0f)
    {
        //ret.append((el->next->elem + el->next->next->elem) * 0.5);
        ret.append(el->next->elem);
        indexRet.append(nPoints-1);
    }
    return true;
}


/*
 * Finds a partitioning by
 * calculating a smoothed curvature (as in Delingette et al.)
 * and finding inflection points (curvature == 0).
 * 
 * @param curvePoints       Points of the curve.
 * @param curvatureSigma    Smoothing parameter (size of neighbourhood)
 * @param curvatureEpsilon  Threshold parameter (see source code)
 * @param ret               Cut points.
 * 
 * @return True if successful, false otherwise.
 */
template <class pointT>
bool goCurvePartition(const goList<pointT>& curvePoints,
                      int                   curvatureSigma,
                      goDouble              curvatureEpsilon,
                      goList<pointT>&       ret)
{
    if (!curvePoints.isClosed())
    {
        goLog::warning("goCurvePartition(): curve must be closed.");
        return false;
    }
    
    //= Get a smoothed curvature approximation of the curve.
    goFixedArray<goDouble> smoothedCurvature(curvePoints.getSize());
    if (!goCurveCurvature(curvePoints, curvatureSigma, smoothedCurvature))
    {
        return false;
    }

    //= Find possible inflection points.
    typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
    goIndex_t i = 0;
    goIndex_t nPoints = (goIndex_t)curvePoints.getSize();
    while (el && i < nPoints-1)
    {
        goDouble f = smoothedCurvature[i] * smoothedCurvature[i] +
                     smoothedCurvature[i+1] * smoothedCurvature[i+1];
        if (smoothedCurvature[i] * smoothedCurvature[i+1] < 0.0 &&
            f > curvatureEpsilon)
        {
            ret.append((el->elem + el->next->elem) * 0.5);
        }
        el = el->next;
        ++i;
    }
    return true;
}

/*
 * Finds inflection points by smoothing the sign(.) function multiple times
 * with a Gaussian.
 ----------------------------------------------------------------------------*/
template <class pointT>
bool goCurvePartitionLabelFiltering(const goList<pointT>& curvePoints,
                                    goIndex_t             filterCount,
                                    goList<pointT>&       ret)
{
    if (!curvePoints.isClosed())
    {
        goLog::warning("goCurvePartition(): curve must be closed.");
        return false;
    }
    
    //= Get a smoothed curvature approximation of the curve.
    goFixedArray<goDouble> curvature(curvePoints.getSize());
    if (!goCurveCurvature(curvePoints, curvature))
    {
        return false;
    }

    goIndex_t nPoints = (goIndex_t)curvePoints.getSize();
    goSignal3D<void> label; 
    label.setDataType (GO_FLOAT);
    label.make (nPoints, 1, 1, nPoints, 1, 1, 5, 0, 0);
    goFloat* labelP = (goFloat*)label.getPtr();
    goIndex_t i;
    
    for (i = 0; i < nPoints; ++i)
    {
        labelP[i] = curvature[i] < 0.0 ? -1.0f : 1.0f;
    }
   
    goFloat maskp[] = {1.0f, 4.0f, 6.0f, 4.0f, 1.0f};
    goFilter1D gauss (maskp, 5, 2, true);

    goSignal3D<void> label2;
    label2.setDataType(GO_FLOAT);
    label2.make(&label);
    goCopySignal (&label, &label2);    

    {
        //= Filter the current sign() function a few times
        //= to smooth out quickly varying regions
        for (goIndex_t j = 0; j < filterCount; ++j)
        {
            if (!gauss.filter(label))
            {
                goLog::warning("goCurvePartition(): gauss filter failed.");
                return false;
            }
        }
        //= Take sign(filtered_labels);
        for (i = 0; i < nPoints; ++i)
        {
            labelP[i] = (labelP[i] < 0.0f ? -1.0f : 1.0f);
        }
        //= Plot (debugging)
        goString f1,f2;
        {
            goFixedArray<goFloat> plot (nPoints);
            goCopySignalArray (&label, plot.getPtr());
            // goPlot::gnuplot (plot,f1,f2,"label",true);
        }
    }

    //= Find possible inflection points.
    typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
    i = 0;
    while (el && i < nPoints-1)
    {
        if (labelP[i] * labelP[i+1] < 0.0f)
        {
            ret.append((el->elem + el->next->elem) * 0.5);
        }
        el = el->next;
        ++i;
    }
    return true;
}


template <class pointT>
bool goLocalMeanCurvature (typename goList<pointT>::ConstElement* curvePointsBegin,
                           goIndex_t             pointCount,
                           bool                  closed,
                           goSize_t              neighSize,
                           goUInt32              filterCount,
                           goFixedArray<goDouble>& meanCurvatureRet)
{
    //= Get a curvature approximation of the curve.
    meanCurvatureRet.setSize(pointCount);
    if (!goCurveCurvature<pointT>(curvePointsBegin, pointCount, meanCurvatureRet, !closed))
    {
        return false;
    }

    //= Replace the curvature by its mean over neighbouring values.
    {
        goFixedArray<goFloat> mask (neighSize*2+1);
        mask.fill(1.0f);
        goFilter1D mean (mask.getPtr(),mask.getSize(),mask.getSize()/2,true);
        goSignal3D<void> cs;
        cs.setDataType(GO_DOUBLE);
        cs.make(pointCount,1,1,pointCount,1,1,mask.getSize(),0,0);
        goCopySignalArray(meanCurvatureRet.getPtr(),&cs);\
        goUInt32 i;
        for (i = 0; i < filterCount; ++i)
        {
            mean.filter(cs);
        }
        goCopySignalArray(&cs,meanCurvatureRet.getPtr());
    }
    return true;
}

/*
 * @brief Calculates local mean curvature and finds inflection points.
 *
 * Calculates a mean curvature over a local neighbourhood
 * (e.g. 11 curve points) and returns the inflection points of the
 * smoothed curvature.
 * 
 * @param curvePoints Points of the 2D curve.
 * @param neighSize   Size of the neighbourhood to calculate the
 *                    mean over. The mean is calculated
 *                    for each point p in the interval 
 *                    [p-neighSize,p+neighsize].
 * @param filterCount The mean is calculated with a linear filter
 *                    which is run filterCount times over the
 *                    curvature.
 * @param ret         Found inflection points. These are the
 *                    points i from curvePoints for which
 *                    curvature[i]*curvature[i+1] < 0.
 *                    The indices into the curvePoints list is also
 *                    returned in indexRet.
 * @param indexRet    Indices into the curvePoints list of
 *                    the points returned in ret.
 * @param curvatureRet Pointer to a goFixedArray<goDouble>
 *                     that contains the curve's
 *                     curvature after the function returns.
 *                     Leave 0 (default) if not needed.
 * @param meanCurvatureRet Pointer to a goFixedArray<goDouble>
 *                     that contains the locally mean filtered
 *                     curvature after the function returns.
 *                     Leave 0 (default) if not needed.
 * 
 * @return True if successful, false otherwise.
 */
template <class pointT>
bool goCurvePartitionLocalMeanCurvature(const goList<pointT>& curvePoints,
                                        goSize_t              neighSize,
                                        goUInt32              filterCount,
                                        goList<pointT>&       ret,
                                        goList<goIndex_t>&    indexRet,
                                        goFixedArray<goDouble>* curvatureRet,
                                        goFixedArray<goDouble>* meanCurvatureRet)
{
    if (!curvePoints.isClosed())
    {
        goLog::warning("goCurvePartitionLocalMeanCurvature(): curve must be closed.");
        return false;
    }
    
    if (curvePoints.getSize() == 0)
    {
        return false;
    }
    
    //= Get a curvature approximation of the curve.
    goFixedArray<goDouble> curvature(curvePoints.getSize());
    if (curvatureRet)
    {
        if (!goCurveCurvature(curvePoints, curvature))
        {
            return false;
        }
    }
//    if (curvatureRet)
//    {
//        *curvatureRet = curvature;
//    }
    
    if (!goLocalMeanCurvature<pointT> (curvePoints.getFrontElement(), curvePoints.getSize(), curvePoints.isClosed(), neighSize, filterCount, curvature))
    {
        return false;
    }
    
    //= Replace the curvature by its mean over neighbouring values.
//    {
//        goFixedArray<goFloat> mask (neighSize*2+1);
//        mask.fill(1.0f);
//        goFilter1D mean (mask.getPtr(),mask.getSize(),mask.getSize()/2,true);
//        goSignal3D<void> cs;
//        cs.setDataType(GO_DOUBLE);
//        cs.make(nPoints,1,1,nPoints,1,1,mask.getSize(),0,0);
//        goCopySignalArray(curvature.getPtr(),&cs);
//        goUInt32 i;
//        for (i = 0; i < filterCount; ++i)
//        {
//            mean.filter(cs);
//        }
//        goCopySignalArray(&cs,curvature.getPtr());
//    }
   
    if (meanCurvatureRet)
    {
        *meanCurvatureRet = curvature;
    }
//    goPlot::gnuplot(curvature,"Curvature","with lines\n","set terminal postscript","> /home/christian/curvature_mean.ps");
    
    return findInflexionPoints (curvature, 
                                curvePoints, 
                                ret, 
                                indexRet);
    return true;
}

template <class pointT>
bool goCurvePartitionDelingette (const goList<pointT>& curvePoints, 
                                 goSize_t neighSize,
                                 goSize_t iterations,
                                 goList<pointT>& ret,
                                 goList<goIndex_t>& indexRet)
{
    goFixedArray<pointT> normalFlow;
    goFixedArray<pointT> tangentFlow;
    goList<pointT> points;
    points = curvePoints;
    for (goIndex_t j = 0; j < (goIndex_t)iterations; ++j)
    {
        goCurvatureDiffusionFlow (points, neighSize, normalFlow, tangentFlow);
        assert (normalFlow.getSize() == (goSize_t)points.getSize());
        typename goList<pointT>::Element* el = points.getFrontElement();
        goIndex_t sz = normalFlow.getSize();
        for (goIndex_t i = 0; i < sz && el; ++i, el = el->next)
        {
            el->elem += normalFlow[i] * 0.1 + tangentFlow[i] * 0.1;
            // printf ("%f %f    %f\n", normalFlow[i].x, normalFlow[i].y, normalFlow[i].abs());
        }
        //matlab.put2DPoints(points, "p");
        //matlab.matlabCall("plot(p(1,1:end),p(2,1:end)); drawnow;");
        // printf ("Iteration %d\n",j);
    }
    goFixedArray<goDouble> curvature(points.getSize());
    if (!goCurveCurvature(points, curvature))
    {
        return false;
    }

    return findInflexionPoints (curvature, 
                                curvePoints, 
                                ret, 
                                indexRet);
}

/*
 * @brief Calculates inflection points by using an upper and
 * a lower trigger level instead of curvature == 0.
 * 
 * Does not work well.
 * 
 * @param curvePoints 
 * @param triggerLevelPos 
 * @param triggerLevelNeg 
 * @param ret 
 * 
 * @return True if successful, false otherwise.
 */
template <class pointT>
bool goCurvePartitionTrigger(const goList<pointT>& curvePoints,
                             goDouble              triggerLevelPos,
                             goDouble              triggerLevelNeg,
                             goList<pointT>&       ret)
{
    if (!curvePoints.isClosed())
    {
        goLog::warning("goCurvePartitionTrigger(): curve must be closed.");
        return false;
    }
    
    if (curvePoints.getSize() == 0)
    {
        return false;
    }
    
    //= Get a curvature approximation of the curve.
    goFixedArray<goDouble> curvature(curvePoints.getSize());
    if (!goCurveCurvature(curvePoints, curvature))
    {
        return false;
    }

    goIndex_t nPoints = (goIndex_t)curvePoints.getSize();

    //= Replace the curvature by its mean over neighbouring values.
    #if 0
    {
        goFloat mask[] = {1.0f,1.0f,1.0f,1.0f,1.0f};
        goFilter1D mean (mask,5,2,true);
        goSignal3D<void> cs;
        cs.setDataType(GO_DOUBLE);
        cs.make(nPoints,1,1,nPoints,1,1,5,0,0);
        goCopySignalArray(curvature.getPtr(),&cs);  
        mean.filter(cs);
        mean.filter(cs);
        goCopySignalArray(&cs,curvature.getPtr());
    }
    #endif
    
    goPlot::gnuplot(curvature,"Curvature");
    
    goIndex_t i;

    //= Find possible inflection points.
    typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
    i = 0;
    goFixedArray<goFloat> label (nPoints);
    goFloat sign = curvature[0] < 0.0 ? -1.0f : 1.0;
    while (el && i < nPoints)
    {
        if (curvature[i] <= triggerLevelNeg)
        {
            if (sign > 0.0f)
            {
                ret.append(el->elem);
                sign = -1.0f;
            }
        }
        else
        {
            if (curvature[i] >= triggerLevelPos)
            {
                if (sign < 0.0f)
                {
                    ret.append(el->elem);
                    sign = 1.0f;
                }
            }
        }
        el = el->next;
        ++i;
    }
    return true;
}

/*
 * @brief Calculate local mean curvature.
 *
 * As in Delingette et al.
 * Check if their formula really makes sense.
 * 
 * @note Deprecated.
 * 
 * @param curvePoints 
 * @param curvatureSigma 
 * @param curvRet 
 * 
 * @return True if successful, false otherwise.
 */
template <class pointT>
bool goCurveCurvature (const goList<pointT>&   curvePoints,
                       int                     curvatureSigma,
                       goFixedArray<goDouble>& curvRet)
{
    if (!curvePoints.isClosed())
    {
        goLog::warning("goCurveCurvature() currently only works for closed point lists (closed curves).");
        return false;
    }
    if (curvRet.getSize() != (goSize_t)curvePoints.getSize())
    {
        curvRet.setSize(curvePoints.getSize());
    }
    goIndex_t i = 0;
    goIndex_t nPoints = (goIndex_t)curvePoints.getSize();
    typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
    while (el && i < nPoints)
    {
        goIndex_t j = 0;
        typename goList<pointT>::Element* el_left  = el->prev;
        typename goList<pointT>::Element* el_right = el->next;
        goDouble l_left      = 0.0;
        goDouble l_right     = 0.0;
        goDouble enumerator  = 0.0;
        goDouble denominator = 0.0;
        while (el_left && el_right && j < curvatureSigma)
        {
            pointT p = el_left->elem - el_left->next->elem;
            l_left  += p.abs();
            p        = el_right->elem - el_right->prev->elem;
            l_right += p.abs();
            goDouble k_left = goCurvature(el_left->prev->elem, 
                                    el_left->elem, 
                                    el_left->next->elem);
            goDouble k_right = goCurvature(el_right->prev->elem, 
                                     el_right->elem, 
                                     el_right->next->elem);
            enumerator += k_left*l_right + k_right*l_left;
            denominator += l_left + l_right;
            
            el_left = el_left->prev;
            el_right = el_right->next;
            ++j;
        }
        assert (denominator != 0.0);
        curvRet[i] = enumerator / denominator;

        el = el->next;
        ++i;
    }
    return true;
}



template 
bool goCurvePartition (const goList<goPointd>&, int, goDouble, goList<goPointd>&);
template 
bool goCurvePartition (const goList<goPointf>&, int, goDouble, goList<goPointf>&); 
template 
bool goCurvePartitionLabelFiltering(const goList<goPointf>& curvePoints,
                                    goIndex_t,
                                    goList<goPointf>&       ret);
template 
bool goCurvePartitionLabelFiltering(const goList<goPointd>& curvePoints,
                                    goIndex_t,
                                    goList<goPointd>&       ret);
template 
bool goCurvePartitionLocalMeanCurvature(const goList<goPointf>& curvePoints,
                                        goSize_t              neighSize,
                                        goUInt32              filterCount,
                             goList<goPointf>&       ret,
                             goList<goIndex_t>&      indexRet,
                             goFixedArray<goDouble>*,
                             goFixedArray<goDouble>*);
template 
bool goCurvePartitionLocalMeanCurvature(const goList<goPointd>& curvePoints,
                                        goSize_t              neighSize,
                                        goUInt32              filterCount,
                             goList<goPointd>&       ret,
                             goList<goIndex_t>&      indexRet,
                             goFixedArray<goDouble>*,
                             goFixedArray<goDouble>*);

template 
bool goCurvePartitionDelingette (const goList<goPointf>& , 
                                 goSize_t ,
                                 goSize_t ,
                                 goList<goPointf>& ,
                                 goList<goIndex_t>& );
template 
bool goCurvePartitionDelingette (const goList<goPointd>& , 
                                 goSize_t ,
                                 goSize_t ,
                                 goList<goPointd>& ,
                                 goList<goIndex_t>& );

template 
bool goCurvePartitionTrigger(const goList<goPointf>& curvePoints,
                             goDouble              triggerLevelPos,
                             goDouble              triggerLevelNeg,
                             goList<goPointf>&       ret);
template 
bool goCurvePartitionTrigger(const goList<goPointd>& curvePoints,
                             goDouble              triggerLevelPos,
                             goDouble              triggerLevelNeg,
                             goList<goPointd>&       ret);

/* How does this work template-wise???? 
 * Answer: When calling, must be explicitly instantiated with e.g. goCurveCurvature<goPointf>(...). WHY? */
//bool goCurveCurvature (goList<goPointf>::ConstElement*, 
//                       goIndex_t,
//                       goVector<goDouble>&, 
//                       bool);
//template bool goCurveCurvature (goList<goPointf>::ConstElement*, 
//                       goIndex_t,
//                       goFixedArray<goDouble>&, 
//                       bool);
//template bool goCurveCurvature (goList<goPointd>::ConstElement*, 
//                       goIndex_t,
//                       goFixedArray<goDouble>&, 
//                       bool);
//template 
//bool goCurveCurvature (goList<goPointd>::ConstElement*, 
//                       goIndex_t,
//                       goVector<goDouble>&, 
//                       bool assumeConstantBoundary);
