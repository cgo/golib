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

/*
 * @brief Get the turn angle between two segments.
 * 
 * @param p1 Point 1 (leftmost point)
 * @param p2 Point 2 (middle point)
 * @param p3 Point 3 (rightmost point)
 * 
 * @return Turn angle in radians.
 */
template <class pointT>
static goDouble getTurn (const pointT& p1, const pointT& p2, const pointT& p3);

/*
 * @brief Approximation of the local curvature.
 * 
 * @param p1 Point 1 (leftmost point)
 * @param p2 Point 2 (middle point)
 * @param p3 Point 3 (rightmost point)
 * 
 * @return Approximation of the curvature in the 3-point segment.
 */
template <class pointT>
static goDouble kappa (const pointT& p1, const pointT& p2, const pointT& p3);

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

/*
 * @brief Approximate local curvature of a curve.
 * 
 * @param begin       First list element of curve points.
 * @param pointCount  Number of points.
 * @param curvRet     Curvature (size will be pointCount).
 * @param assumeConstantBoundary If true (default), one boundary point each at
 *                               beginning and end of the list will be 
 *                               assumed to be the boundary points. This is like for
 *                               open curves.
 *                               If false, there must be one point in 
 *                               begin->prev and one next to the last point.
 * 
 * @return True if successful, false otherwise.
 */
// FIXME: What the hell is wrong with this template???
template <class pointT>
bool goCurveCurvature (typename goList<pointT>::ConstElement* begin,
                       goIndex_t pointCount,
                       goFixedArray<goDouble>& curvRet,
                       bool assumeConstantBoundary)
{
    if (curvRet.getSize() != static_cast<goSize_t>(pointCount))
    {
        curvRet.setSize(pointCount);
    }
    //= "Closed curves"
    if (!assumeConstantBoundary)
    {
        goIndex_t i = 0;
        typename goList<pointT>::ConstElement* el = begin;
        //goList<goPointf>::ConstElement* el = begin;
        while (el && i < pointCount)
        {
            curvRet[i] = kappa(el->prev->elem, 
                    el->elem, 
                    el->next->elem);
            el = el->next;
            ++i;
        }
        return true;
    }
    //= "Open curves"
    else
    {
        goIndex_t i = 0;
        if (pointCount < 3)
        {
            for (i = 0; i < pointCount; ++i)
            {
                curvRet[i] = 0.0;
            }
            return true;
        }
        typename goList<pointT>::ConstElement* el = begin;
        //goList<goPointf>::ConstElement* el = begin;
        assert (el);
        curvRet[0] = kappa(el->elem, el->elem, el->next->elem);
        el = el->next;
        for (i = 1; i < pointCount - 1 && el; ++i)
        {
            curvRet[i] = kappa(el->prev->elem, el->elem, el->next->elem);
            el = el->next;
        }
        curvRet[pointCount-1] = kappa(el->prev->elem, el->elem, el->elem);
        return true;
    }
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
    
    goIndex_t nPoints = (goIndex_t)curvePoints.getSize();

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
    
    //= Find possible inflection points.
    typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
    goIndex_t i = 0;
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
 * @brief Approximate local curvature of a curve.
 * 
 * @note Works for closed lists, hast not been tested for open.
 * 
 * @param curvePoints Curve points (must be a closed list).
 * @param curvRet     Curvature (same size as points).
 * 
 * @return True if successful, false otherwise.
 */
template <class pointT>
bool goCurveCurvature (const goList<pointT>&   curvePoints,
                       goFixedArray<goDouble>& curvRet)
{
    if (curvRet.getSize() != (goSize_t)curvePoints.getSize())
    {
        curvRet.setSize(curvePoints.getSize());
    }
    //= Closed curves
    if (curvePoints.isClosed())
    {
        goIndex_t i = 0;
        goIndex_t nPoints = static_cast<goIndex_t>(curvePoints.getSize());
        typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
        while (el && i < nPoints)
        {
            curvRet[i] = kappa(el->prev->elem, 
                    el->elem, 
                    el->next->elem);
            el = el->next;
            ++i;
        }
        return true;
    }
    //= Open curves
    else
    {
        goIndex_t i = 0;
        goIndex_t nPoints = static_cast<goIndex_t>(curvePoints.getSize());
        if (nPoints < 3)
        {
            for (i = 0; i < nPoints; ++i)
            {
                curvRet[i] = 0.0;
            }
            return true;
        }
        typename goList<pointT>::ConstElement* el = curvePoints.getFrontElement();
        assert (el);
        curvRet[0] = kappa(el->elem, el->elem, el->next->elem);
        el = el->next;
        for (i = 1; i < nPoints - 1 && el; ++i)
        {
            curvRet[i] = kappa(el->prev->elem, el->elem, el->next->elem);
            el = el->next;
        }
        el = curvePoints.getTailElement();
        curvRet[nPoints-1] = kappa(el->prev->elem, el->elem, el->elem);
        return true;
    }
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
            goDouble k_left = kappa(el_left->prev->elem, 
                                    el_left->elem, 
                                    el_left->next->elem);
            goDouble k_right = kappa(el_right->prev->elem, 
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

static goDouble kappa (goDouble phi, goDouble r)
{
    return sin(phi) / r;
}

template <class pointT>
static goDouble kappa (const pointT& p1, const pointT& p2, const pointT& p3)
{
    goDouble phi = getTurn(p1,p2,p3);
    pointT p = p3 - p1;
    return kappa(phi,p.abs()*0.5);
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
    if (l1 == 0.0 || l2 == 0.0)
    {
        return 0.0;
    }
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

    return beta * ((s1.x * s2.y < s1.y * s2.x) ? 1.0 : -1.0);
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
