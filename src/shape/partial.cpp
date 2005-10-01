#include <goarray.h>
#include <golist.h>
#include <gotypes.h>
#include <gopoint.h>
#include <govector.h>
#include <gocurve.h>
#include <goshape.h>
#include <golog.h>

/**
* @brief Extremely simple version of partitioning of a curve.
*
* @param partEndRet  After returning, contains indices into the point list to 
*                    points which are considered to partition the curve.
* @param points      Points on the curve.
*
* @return Number of partitioning points found (partEndRet.getSize()).
**/
template <class pointT>
goIndex_t getConvexConcaveParts (goArray<goIndex_t>& partEndRet, goList<pointT>& points)
{
    goVectord normal (2);
    normal.fill (0.0);
    partEndRet.resize (0);

    typename goList<pointT>::ConstElement* pointsEl = points.getFrontElement();

    goIndex_t i = 0;
    goIndex_t sz = points.getSize();
    pointT p1;
    pointT p2;
    pointT p3;
    pointT l;
    pointT d;
    goList<pointT> dList;
    //= Account for closed lists: check for list size too.
    while (pointsEl && i < sz)
    {
        p1 = pointsEl->elem;
        if (pointsEl->next)
        {
            p2 = pointsEl->next->elem;
            if (pointsEl->next->next)
                p3 = pointsEl->next->next->elem;
            else
                p3 = p2;
        }
        else
        {
            p2 = p1;
            p3 = p2;
        }

        //= Calculate the direction of curvature:
        l = (p3 - p1);
        if (l.abs() != 0.0)
        {
            l /= l.abs();
            d = (p2 - p1) - l * ((p2 - p1) * l);
            dList.append(d);
        }
        
        pointsEl = pointsEl->next;
        ++i;
    }
    
    {
        typename goList<pointT>::Element* dEl = dList.getFrontElement();
        if (!dEl->next)
            return 0;
        goDouble f;
        i = 0;
        goIndex_t j = 4;
        while (dEl)
        {
            if (dEl->next)
            {
                f = dEl->elem * dEl->next->elem;
                if (j > 0)
                    --j;
                if (j <= 0 && f < 0)
                {
                    partEndRet += i+1;
                    j = 4;
                }
            }
            dEl = dEl->next;
            ++i;
        }
    }

    return partEndRet.getSize();
}

template goIndex_t getConvexConcaveParts<goPointf> (goArray<goIndex_t>& partEndRet, goList<goPointf>& points);
template goIndex_t getConvexConcaveParts<goPointd> (goArray<goIndex_t>& partEndRet, goList<goPointd>& points);

//= Latecki's convexity rules

//= Relevance measure K of two neighbouring digital curve segments p1 -- p2 -- p3
template <class pointT>
goDouble relevanceMeasure (const pointT& p1, const pointT& p2, const pointT& p3, goDouble totalCurveLength)
{
    if (totalCurveLength <= 0.0)
        return 0.0;
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
    goDouble alpha1 = acos(s1 * base / l1);
    goDouble alpha2 = acos(-(s2 * base / l2));
    goDouble beta = alpha1 + alpha2;

    f = 1.0 / totalCurveLength;
    l1 *= f;
    l2 *= f;
    return beta * l1 * l2 / (l1 + l2);
}

//= Simple test. Just do one single step.
template <class T>
bool goLinearizeCurve (goCurve<goPoint<T> >& curve)
{
    goList<goPoint<T> >* points = &curve.getPoints();
    assert (points);
    if (points->getSize() < 3)
        return false;

    typename goList<goPoint<T> >::Element* el = points->getFrontElement();
    
    goIndex_t i = 0;
    goIndex_t m = points->isClosed() ? 1 : 2;
    goDouble length = curve.getLength();
    typename goList<goPoint<T> >::Element* min_el = el;
    goDouble min_relevance = relevanceMeasure(el->elem, el->next->elem, el->next->next->elem, length);
    while ((i < points->getSize() - m) && el)
    {
        goDouble rel = relevanceMeasure(el->elem, el->next->elem, el->next->next->elem, length);
        if (rel < min_relevance)
        {
            min_relevance = rel;
            min_el = el;
        }
        el = el->next;
        ++i;
    }
    if (min_el && min_el->next)
    {
        goLog::message ("goLinearizeCurve(): Removing an element!");
        points->remove (min_el->next);
    }
    return true;
}

template goDouble relevanceMeasure (const goPointf& p1, const goPointf& p2, const goPointf& p3, goDouble totalCurveLength);
template goDouble relevanceMeasure (const goPointd& p1, const goPointd& p2, const goPointd& p3, goDouble totalCurveLength);
template bool     goLinearizeCurve   (goCurve<goPoint<goFloat> >& curve);
template bool     goLinearizeCurve   (goCurve<goPoint<goDouble> >& curve);

#if 0
template<class pointT>
goIndex_t getPartialCurve (goList<pointT>::ConstElement* curvePoints,
                           goFixedArray<pointT>&         partRet)
{
    goIndex_t length = static_cast<goIndex_t>(partRet.getSize());
    goList<pointT>::ConstElement* curveEl = curvePoints;
   
    goIndex_t i = 0;
    while (curveEl && i < length)
    {
        partRet[i] = curveEl->elem;
        curveEl = curveEl->next;
        ++i;
    }
    //= Return number of points actually copied to partRet.
    return i;
}

template<class pointT>
bool getPartialCurves (goList<pointT>&        curve,
                       goIndex_t              partLength)
{
    goList<pointT>::ConstElement* curveEl    = curve.getFrontElement();
    goList<pointT>::Element*      curveFront = curve.getFrontElement();
    goList< goFixedArray<pointT> >  parts;
    
    goFixedArray<pointT> part (partLength);
    bool wasClosed = curve.getPoints().isClosed();
    if (!wasClosed)
    {
        curve.close();
    }
   
    goIndex_t i = 0;
    goIndex_t length = curve.getSize();
    for (i = 0; i < length; ++i)
    {
        if (getPartialCurve(curveEl, part) != partLength)
        {
            assert ("partLength != lenght of part" == 0);
        }
        parts.append (part);
    }
    
    if (!wasClosed)
    {
        curve.open (curveFront);
    }
}

class goShapePartsPrivate;

template <class pointT>
class goShapeParts : public goObjectBase
{
    public:
        goShapeParts ();
        virtual ~goShapeParts ();

        

    private:
        goShapePartsPrivate* myPrivate;
};

template <class pointT>
class goShapePartsPrivate
{
    public:
        goShapePartsPrivate () {};
        ~goShapePartsPrivate () {};

        goList< goFixedArray<pointT> >  partList;
};
#endif
