#include <godiscretecurvepartition.h>
#include <assert.h>
#ifndef GOMATH_H
# include <gomath.h>
#endif

template <class T>
class goDiscreteCurvePartitionPrivate
{
    public:
        goDiscreteCurvePartitionPrivate() 
            : curve(0) 
            {};
        ~goDiscreteCurvePartitionPrivate() {};

        goCurve<goPoint<T> >* curve;
        
};

template <class T>
goDiscreteCurvePartition<T>::goDiscreteCurvePartition ()
    : goObjectBase (),
      myPrivate    (0)
{
    myPrivate = new goDiscreteCurvePartitionPrivate<T>;
    this->setClassName ("goDiscreteCurvePartition");
}

template <class T>
goDiscreteCurvePartition<T>::~goDiscreteCurvePartition ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

template <class T>
void goDiscreteCurvePartition<T>::setCurve (goCurve<goPoint<T> >* curve)
{
    myPrivate->curve = curve;
}

template <class T>
goCurve<goPoint<T> >* goDiscreteCurvePartition<T>::getCurve ()
{
    return myPrivate->curve;
}

/**
* @brief  Partition a curve in line segments.
*
* Simplifies the polygon of curve points until
* the number of polygon points is lower or equal minPoints
* or the polygon is convex.
*
* @param minPoints  Threshold for the number of points
*                   we want the curve polygon to contain.
* @param maxLength  Maximal length of segments that can be removed.
*                   All segments larger or equal this length are retained.
*                   The length is absolute, not normalised to the curve length.
*                   A negative length (default) means that all segments can be
*                   removed.
* @todo Inefficient. See source code and fix when needed.
* 
* @return True if successful, false otherwise.
**/
template <class T>
bool goDiscreteCurvePartition<T>::lateckiSimplify (goIndex_t minPoints, goDouble maxLength)
{
    if (!myPrivate->curve)
        return false;

    goCurve<goPoint<T> >* curve = myPrivate->curve;
    
    goList<goPoint<T> >* points = &curve->getPoints();
    assert (points);
    if (points->getSize() < minPoints)
        return true;

    bool isConvex = false;
    bool removedPoint = true;
    //= NOTE: This is not efficient. Sorting the line segments with respect to the relevance measure
    //=       can speed this up significantly. Do so when needed.
    while (points->getSize() > minPoints && !isConvex && removedPoint)
    {
        removedPoint = false;
        typename goList<goPoint<T> >::Element* el = points->getFrontElement();
        
        goIndex_t i = 0;
        goIndex_t m = points->isClosed() ? 0 : 2;
        goDouble length = curve->getLength();
        typename goList<goPoint<T> >::Element* min_el = el->next;
        goDouble beta(0.0);
        goDouble maxSegmentLength = 0.0;
        goDouble min_relevance = lateckiRelevanceMeasure(el->elem, el->next->elem, el->next->next->elem, length, beta, maxSegmentLength);
        if (maxSegmentLength > maxLength)
        {
            min_el = 0;
        }
        goDouble currentBeta(0.0);
        isConvex = true;
        while ((i < points->getSize() - m) && el)
        {
            goDouble rel = lateckiRelevanceMeasure(el->elem, el->next->elem, el->next->next->elem, length, currentBeta, maxSegmentLength);
            // printf ("\t%.5f\n", (currentBeta) * (beta));
            el->next->elem.value = currentBeta;
            // printf ("\t%.5f\n", rel);
            if ((currentBeta) * (beta) < 0.0)
            {
                isConvex = false;
            }
            beta = currentBeta;
            if (rel < min_relevance)
            {
                if (maxLength < 0.0)
                {
                    min_relevance = rel;
                    min_el = el->next;
                }
                else
                {
                    if (maxSegmentLength < maxLength)
                    {
                        min_relevance = rel;
                        min_el = el->next;
                    }
                }
            }
            el = el->next;
            ++i;
        }
        if (min_el && !isConvex)
        {
            points->remove (min_el);
            removedPoint = true;
        }
    }
    return true;
}

template <class T>
goDouble turnAngle (const goPoint<T>& p1, const goPoint<T>& p2, const goPoint<T>& p3)
{
    //= ...
    return -1.0;
}

//= Relevance measure K of two neighbouring digital curve segments p1 -- p2 -- p3
//= For consecutive line segments s1 and s2,
//= K := angle(s1,s2)*l(s1)*l(s2) / (l(s1) + l(s2))
//= l(s) is the length of s normalised w.r.t. the curve.
template <class T>
goDouble goDiscreteCurvePartition<T>::lateckiRelevanceMeasure (const goPoint<T>& p1, const goPoint<T>& p2, const goPoint<T>& p3, goDouble totalCurveLength, goDouble& betaRet, goDouble& maxSegmentLength)
{
    if (totalCurveLength <= 0.0)
        return 0.0;
    goPoint<T> base = p3 - p1;
    goDouble f = base.abs();
    if (f == 0.0)
    {
        assert ("p3 == p1" == 0);
        return 0.0;  //= This should not happen. It would mean p3 == p1.
    }
    base *= 1.0 / f;
    goPoint<T> s1 = p2 - p1;
    goPoint<T> s2 = p3 - p2;
    goDouble l1 = s1.abs();
    goDouble l2 = s2.abs();
    assert (l1 != 0.0 && l2 != 0.0);
    maxSegmentLength = goMath::max(l1,l2);
    
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

    f = 1.0 / totalCurveLength;
    l1 *= f;
    l2 *= f;
    //= The sign of the turn angle is determined by whether the triangle {p1,p2,p3} 
    //= turns clockwise or counter-clockwise, which in turn is determined
    //= by the z-component of the cross product {s1 0} x {s2 0}.
    betaRet = beta * ((s1.x * s2.y < s1.y * s2.x) ? -1 : 1);
    return beta * l1 * l2 / (l1 + l2);
}

template class goDiscreteCurvePartition<goFloat>;
template class goDiscreteCurvePartition<goDouble>;
