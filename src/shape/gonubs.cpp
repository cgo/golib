#include <assert.h>
#include <gofixedarray.h>
#include <golist.h>
#include <gopoint.h>
#include <gocurve.h>
#include <gonubs.h>
#include <golog.h>
#include <gomath.h>

class goNUBSPrivate
{
    public:
        goNUBSPrivate();
        ~goNUBSPrivate();
        
        goList<goPointf>        controlPoints;
        goDouble                curveLength;
        goArray<goDouble>       knotValues;
};

goNUBSPrivate::goNUBSPrivate()
    : controlPoints (),
      curveLength   (0.0),
      knotValues    ()
{
}


goNUBSPrivate::~goNUBSPrivate()
{
}

//===================================================

/**
* @brief Constructor.
* 
* @param curve  Pointer to the curve object. If 0, no action is taken. If != 0, the
*               curve is approximated and operator() can be used to evaluate the curve.
*               The valid parameter range is [0,getCurveLength()).
**/
goNUBS::goNUBS(const goCurvef* curve)
    : myPrivate (0)
{
    myPrivate = new goNUBSPrivate;
    assert (myPrivate);
    if (curve)
    {
        this->setControlPoints (curve->getPoints());
    }
}

/**
* @brief Constructor.
* 
* @param curve  Pointer to the curve object. If 0, no action is taken. If != 0, the
*               curve is approximated and operator() can be used to evaluate the curve.
*               The valid parameter range is [0,getCurveLength()).
**/
goNUBS::goNUBS(const goCurved* curve)
    : myPrivate (0)
{
    myPrivate = new goNUBSPrivate;
    assert (myPrivate);
    if (curve)
    {
        this->setControlPoints (curve->getPoints());
    }
}


goNUBS::goNUBS ()
    : myPrivate (0)
{
    myPrivate = new goNUBSPrivate;
    assert (myPrivate);
}

goNUBS::~goNUBS()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}


/**
* @brief Get curve length.
*
* The valid parameter range for operator() can be determined with this function and is
* [0,getCurveLength()).
* 
* @return The curve length.
**/
goDouble goNUBS::getCurveLength () const
{
    return myPrivate->curveLength;
}


/**
* @brief Calculate with current control points.
*
* Basically only calculates the knot values.
**/
bool goNUBS::calculate ()
{
    if (myPrivate->controlPoints.getSize() < 4)
        return false;

    goList<goPointf>::Element* pointList = myPrivate->controlPoints.getFrontElement();
    assert (pointList);

    goIndex_t knotCount = myPrivate->controlPoints.getSize() + 6;
    myPrivate->knotValues.resize (knotCount);
    myPrivate->knotValues [0] = 0.0;
    myPrivate->knotValues [1] = 0.0;
    myPrivate->knotValues [2] = 0.0;
    myPrivate->knotValues [3] = 0.0;
    
    goIndex_t i;
    goIndex_t controlPointCount = (goIndex_t)myPrivate->controlPoints.getSize();
    goList<goPointf>::Element* pointList2 = pointList->next;
    goFloat accumDist = 0.0f;
    //= Create an array with the accumulated distances between the points:
    for (i = 1; i <= controlPointCount; ++i)
    {
        goFloat temp1 = pointList2->elem.x - pointList->elem.x;
        goFloat temp2 = pointList2->elem.y - pointList->elem.y;
        accumDist += sqrt(temp1 * temp1 + temp2 * temp2);
        myPrivate->knotValues [i + 3] = accumDist;
        // printf ("Knotvalue %f\n", accumDist);
        if (!pointList2->next)
            break;
        pointList2 = pointList2->next;
        pointList = pointList->next;
    }
    myPrivate->knotValues [knotCount - 3] = accumDist;
    myPrivate->knotValues [knotCount - 2] = accumDist;
    myPrivate->knotValues [knotCount - 1] = accumDist;

    if (myPrivate->controlPoints.isClosed())
    {
        goDouble* k = myPrivate->knotValues.getPtr();
        k[2] = -(k[knotCount - 3] - k[knotCount - 4]);
        k[1] = -(k[knotCount - 4] - k[knotCount - 5]);
        k[0] = -(k[knotCount - 5] - k[knotCount - 6]);
        k[knotCount - 3] = k[knotCount - 4] + (k[4] - k[3]);
        k[knotCount - 2] = k[knotCount - 3] + (k[5] - k[4]);
        k[knotCount - 1] = k[knotCount - 2] + (k[6] - k[5]);

        accumDist += -k[knotCount - 3] + k[knotCount - 6] - k[3] + k[6];
        
        //goFloat temp1 = pointList2->elem.x - pointList->elem.x;
        //goFloat temp2 = pointList2->elem.y - pointList->elem.y;
        //accumDist += sqrt(temp1 * temp1 + temp2 * temp2);
    }
    myPrivate->curveLength = accumDist;
    return true;
}

/*
* @brief Calculate cubic spline value
* @todo Works with recursion. Replace with a more efficient algorithm
*       for the cubic case (see literature).
* @param u  Parameter value
* @param t  goDouble* to the 5 knot values
*
* @return The value at u.
*/
static goDouble B_ (goFloat u, goDouble* t, goIndex_t i, goIndex_t m)
{
    if (m <= 0)
    {
        if (t[i] <= u && u < t[i+1])
            return 1.0;
        return 0.0;
    }
    goDouble temp1 = 0.0;
    if (t[i+1] != t[i])
        temp1 = (u - t[i]) / (t[i+m] - t[i]) * B_(u,t,i,m-1);
    goDouble temp2 = 0.0;
    if (t[i+1] != t[i+1+m])
        temp2 = (t[i+m+1] - u) / (t[i+m+1] - t[i+1]) * B_(u,t,i+1,m-1);
    return temp1 + temp2;
}

/**
* @brief Evaluate the B-spline curve at u.
*
* @note Note that parameter u must be in a half-open interval. If you request
*       nubs.operator()(nurbs.getCurveLength()), you will get (0,0,0) as an answer.
*       E.g., nurbs.operator(nurbs.getCurveLength() - 1e-5) results in something 
*       close to the curve's end.
* 
* @todo If it nags someone too much, return the end point when 
*       nubs.operator()(nurbs.getCurveLength()) is requested.
* @todo Currently intervals are searched for linearly. Improve that when there's time.
* @todo Store the control points in an array additionally to speed up access.
* @param u  Point to evaluate at. Must be in [ 0,getCurveLength() ). Note the half-open
*           interval.
*
* @return Point at u. (0,0,0) by default, e.g. if u is out of range.
**/
goPointf goNUBS::operator() (goFloat u)
{
    goPointf p (0.0f,0.0f,0.0f);
    //= Find the basis functions. Ours are in ascending order, so the first with t <= u is the first 
    //= basis.
    u = goMath::min ((goDouble)u,this->getCurveLength());
    goIndex_t i = 3;
    while ((i < myPrivate->knotValues.getSize() - 1))
    {
        if (u >= myPrivate->knotValues[i] && u < myPrivate->knotValues[i+1])
            break;
        ++i;
    }
    if (i >= (myPrivate->knotValues.getSize() - 1))
    {
        goLog::warning ("goNUBS::operator(): could not find a curve element with t < u!");
        return p;
    }

    if (i < 3 || i > myPrivate->controlPoints.getSize() + 3)
    {
        goLog::warning ("goNUBS::operator(): i < 3 or i > num. controlPoints + 3");
        return p;
    }
    goList<goPointf>::Element* pl = myPrivate->controlPoints.getFrontElement();
    {
        goIndex_t temp = i - 3;
        while (temp >= 0)
        {
            if (pl->next)
                pl = pl->next;
            --temp;
        }
    }
        
    goPointf points [4];
    points[0] = pl->elem;
    if (!pl->prev)
    {
        points[1] = pl->elem;
        points[2] = pl->elem;
        points[3] = pl->elem;
    }
    else
    {
        points[1] = pl->prev->elem;
        if (!pl->prev->prev)
        {
           points[2] = points[1];
           points[3] = points[1];
        }
        else
        {
            points[2] = pl->prev->prev->elem;
            if (!pl->prev->prev->prev)
            {
                points[3] = points[2];
            }
            else
            {
                points[3] = pl->prev->prev->prev->elem;
            }
        }
    }
    
    goDouble* t   = myPrivate->knotValues.getPtr();
    p.x = points[3].x * B_(u,t,i-3,3) + 
          points[2].x * B_(u,t,i-2,3) +   
          points[1].x * B_(u,t,i-1,3) +   
          points[0].x * B_(u,t,i,3);
    p.y = points[3].y * B_(u,t,i-3,3) + 
          points[2].y * B_(u,t,i-2,3) +   
          points[1].y * B_(u,t,i-1,3) +   
          points[0].y * B_(u,t,i,3);
    p.w = points[3].w * B_(u,t,i-3,3) + 
          points[2].w * B_(u,t,i-2,3) +   
          points[1].w * B_(u,t,i-1,3) +   
          points[0].w * B_(u,t,i,3);
   
    goFloat f = 1.0f;
    if (p.w != 0.0)
        f /= p.w;
    p *= f;
    return p;
}

/**
* @brief Calculate with given point list.
*
* Copies <code>points</code> to the internal point list,
* adds the last point twice
* and calls calculate().
* 
* @param points  The point list describing the curve to be approximated.
*
* @return True if successful, false otherwise.
**/
bool goNUBS::setControlPoints (const goList<goPointf>& points)
{
    if (points.getSize() < 4)
        return false;

    myPrivate->controlPoints.erase();
    const goPointf* p = &points.getFrontElement()->elem;
    assert (p);
    goPointf pp = *p;
    pp.w = 1.0f;
    goIndex_t sz = points.getSize();
    goIndex_t i = 0;
    goList<goPointf>::ConstElement* el = points.getFrontElement();
//    if (points.isClosed())
//    {
//        pp = el->prev->prev->prev->elem;
//        pp.w = 1.0f;
//        myPrivate->controlPoints.append(pp);
//        pp = el->prev->prev->elem;
//        pp.w = 1.0f;
//        myPrivate->controlPoints.append(pp);
//        pp = el->prev->elem;
//        pp.w = 1.0f;
//        myPrivate->controlPoints.append(pp);
//    }
    while (i < sz)
    {
        pp = el->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
        if (!el->next)
            break;
        el = el->next;
        ++i;
    }
//    if (points.isClosed())
//    {
//        pp = points.getFrontElement()->elem;
//        pp.w = 1.0f;
//        myPrivate->controlPoints.append(pp);
//        pp = points.getFrontElement()->next->elem;
//        pp.w = 1.0f;
//        myPrivate->controlPoints.append(pp);
//        pp = points.getFrontElement()->next->next->elem;
 //       pp.w = 1.0f;
//        myPrivate->controlPoints.append(pp);
//    }
//    else
    if (points.isClosed())
    {
        myPrivate->controlPoints.close();
    }
    else
    {
        pp = points.getTailElement()->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
    }
    this->calculate ();
    return true;
}

bool goNUBS::setControlPoints (const goList<goPointd>& points)
{
    if (points.getSize() < 4)
        return false;

    myPrivate->controlPoints.erase();
    const goPointd* p = &points.getFrontElement()->elem;
    assert (p);
    goPointf pp;
    pp.w = 1.0f;
    goIndex_t sz = points.getSize();
    goIndex_t i = 0;
    goList<goPointd>::ConstElement* el = points.getFrontElement();
    while (i < sz)
    {
        pp = el->elem;
        pp.w = 1.0;
        myPrivate->controlPoints.append(pp);
        if (!el->next)
            break;
        el = el->next;
        ++i;
    }
    if (points.isClosed())
    {
        myPrivate->controlPoints.close();
    }
    else
    {
        pp = points.getTailElement()->elem;
        pp.w = 1.0;
        myPrivate->controlPoints.append(pp);
    }
    this->calculate ();
    return true;
}

/**
* @brief Set control points to given point list.
*
* Copies <code>points</code> to the internal point list,
* adds the last point twice
* and calls calculate().
* 
* @param points  The point list describing the curve to be approximated.
*
* @return True if successful, false otherwise.
**/
bool goNUBS::setControlPoints (goList<goPointf>::ConstElement* begin, goList<goPointf>::ConstElement* end, bool closed)
{
    if (begin == 0 || end == begin)
        return false;
    
    myPrivate->controlPoints.erase();
    const goPointf* p = &begin->elem;
    assert (p);
    goPointf pp = *p;
    pp.w = 1.0f;
    goIndex_t i = 0;
    goList<goPointf>::ConstElement* el = begin;
    while (el != end)
    {
        pp = el->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
        ++i;
        el = el->next;
    }
    if (i < 4)
        return false;

    if (closed)
    {
        myPrivate->controlPoints.close();
    }
    else
    {
        pp = myPrivate->controlPoints.getTailElement()->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
    }
    this->calculate ();
    return true;
}

bool goNUBS::setControlPoints (goList<goPointd>::ConstElement* begin, goList<goPointd>::ConstElement* end, bool closed)
{
//    if (points.getSize() < 4)
//        return false;

    if (begin == 0 || end == begin)
        return false;
    
    myPrivate->controlPoints.erase();
    const goPointd* p = &begin->elem;
    assert (p);
    goPointf pp = *p;
    pp.w = 1.0f;
    goIndex_t i = 0;
    goList<goPointd>::ConstElement* el = begin;
    while (el != end)
    {
        pp = el->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
        ++i;
        el = el->next;
    }
    if (i < 4)
        return false;

    if (closed)
    {
        myPrivate->controlPoints.close();
    }
    else
    {
        pp = myPrivate->controlPoints.getTailElement()->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
    }
    this->calculate ();
    return true;
}

/**
* @brief Set control points to given point list.
*
* Copies <code>points</code> to the internal point list,
* adds the last point twice
* and calls calculate().
* 
* @param points  The point list describing the curve to be approximated.
*
* @return True if successful, false otherwise.
**/
bool goNUBS::setControlPoints (goList<goPointf>::ConstElement* begin, goIndex_t count, bool closed)
{
    if (begin == 0 || count < 4)
        return false;
    
    myPrivate->controlPoints.erase();
    const goPointf* p = &begin->elem;
    assert (p);
    goPointf pp = *p;
    pp.w = 1.0f;
    goIndex_t i = 0;
    goList<goPointf>::ConstElement* el = begin;
    while (el && i < count)
    {
        pp = el->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
        ++i;
        el = el->next;
    }
    if (i < 4)
        return false;

    if (closed)
    {
        myPrivate->controlPoints.close();
    }
    else
    {
        pp = myPrivate->controlPoints.getTailElement()->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
    }
    this->calculate ();
    return true;
}

/**
* @brief Set control points to given point list.
*
* Copies <code>points</code> to the internal point list,
* adds the last point twice
* and calls calculate().
* 
* @param points  The point list describing the curve to be approximated.
*
* @return True if successful, false otherwise.
**/
bool goNUBS::setControlPoints (goList<goPointd>::ConstElement* begin, goIndex_t count, bool closed)
{
    if (begin == 0 || count < 4)
        return false;
    
    myPrivate->controlPoints.erase();
    const goPointd* p = &begin->elem;
    assert (p);
    goPointf pp = *p;
    pp.w = 1.0f;
    goIndex_t i = 0;
    goList<goPointd>::ConstElement* el = begin;
    while (el && i < count)
    {
        pp = el->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
        ++i;
        el = el->next;
    }
    if (i < 4)
        return false;

    if (closed)
    {
        myPrivate->controlPoints.close();
    }
    else
    {
        pp = myPrivate->controlPoints.getTailElement()->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
    }
    this->calculate ();
    return true;
}
