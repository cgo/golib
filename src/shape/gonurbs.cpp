#include <assert.h>
#include <gofixedarray.h>
#include <golist.h>
#include <gopoint.h>
#include <gocurve.h>
#include <gonurbs.h>
#include <golog.h>
#include <gomath.h>

class goNURBSPrivate
{
    public:
        goNURBSPrivate();
        ~goNURBSPrivate();
        
        goList<goPointf>        controlPoints;
        goDouble                curveLength;
        goArray<goDouble>       knotValues;
};

goNURBSPrivate::goNURBSPrivate()
    : controlPoints (),
      curveLength   (0.0),
      knotValues    ()
{
}


goNURBSPrivate::~goNURBSPrivate()
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
goNURBS::goNURBS(const goCurvef* curve)
    : myPrivate (0)
{
    myPrivate = new goNURBSPrivate;
    assert (myPrivate);
    if (curve)
    {
        this->interpolate (curve->getPoints());
    }
}

/**
* @brief Constructor.
* 
* @param curve  Pointer to the curve object. If 0, no action is taken. If != 0, the
*               curve is approximated and operator() can be used to evaluate the curve.
*               The valid parameter range is [0,getCurveLength()).
**/
goNURBS::goNURBS(const goCurved* curve)
    : myPrivate (0)
{
    myPrivate = new goNURBSPrivate;
    assert (myPrivate);
    if (curve)
    {
        this->interpolate (curve->getPoints());
    }
}


goNURBS::goNURBS ()
    : myPrivate (0)
{
    myPrivate = new goNURBSPrivate;
    assert (myPrivate);
}

goNURBS::~goNURBS()
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
goDouble goNURBS::getCurveLength () const
{
    return myPrivate->curveLength;
}


/**
* @brief Calculate with current control points.
*
* Basically only calculates the knot values.
**/
void goNURBS::interpolate ()
{
    if (myPrivate->controlPoints.getSize() < 4)
        return;

    goList<goPointf>::Element* pointList = myPrivate->controlPoints.getFrontElement();
    assert (pointList);

    myPrivate->knotValues.resize (myPrivate->controlPoints.getSize() + 4);
    myPrivate->knotValues [0] = 0.0;
    myPrivate->knotValues [1] = 0.0;
    myPrivate->knotValues [2] = 0.0;
    myPrivate->knotValues [3] = 0.0;
    
    goIndex_t i;
    goIndex_t controlPointCount = (goIndex_t)myPrivate->controlPoints.getSize();
    goList<goPointf>::Element* pointList2 = pointList->next;
    goFloat accumDist = 0.0f;
    //= Create an array with the accumulated distances between the points:
    for (i = 0; i < controlPointCount; ++i)
    {
        goFloat temp1 = pointList2->elem.x - pointList->elem.x;
        goFloat temp2 = pointList2->elem.y - pointList->elem.y;
        accumDist += sqrt(temp1 * temp1 + temp2 * temp2);
        myPrivate->knotValues [i + 4] = accumDist;
        // printf ("Knotvalue %f\n", accumDist);
        if (!pointList2->next)
            break;
        pointList2 = pointList2->next;
        pointList = pointList->next;
    }
    myPrivate->knotValues [controlPointCount + 3] = accumDist;
    // myPrivate->knotValues [controlPointCount + 4] = accumDist;
    // myPrivate->knotValues [controlPointCount + 5] = accumDist;
    // printf ("goNURBS::interpolate(): length: %f\n", accumDist);
    myPrivate->curveLength = accumDist;
}

/*
* @brief Calculate cubic spline value
*
* @param u  Parameter value
* @param t  goFloat* to the 5 knot values
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
* @todo Currently intervals are searched for linearly. Improve that when there's time.
* @param u  Point to evaluate at. Must be in [0,getCurveLength()).
*
* @return Point at u. (0,0,0) by default, e.g. if u is out of range.
**/
goPointf goNURBS::operator() (goFloat u)
{
    goPointf p;
    //= Find the basis functions. Ours are in ascending order, so the first with t <= u is the first 
    //= basis.
    u = goMath::min ((goDouble)u,this->getCurveLength() - 1e-6);
    goIndex_t i = 3;
    while ((i < myPrivate->knotValues.getSize() - 1))
    {
        if (u >= myPrivate->knotValues[i] && u <= myPrivate->knotValues[i+1])
            break;
        ++i;
    }
    if (i >= (myPrivate->knotValues.getSize() - 1))
    {
        goLog::warning ("goNURBS::operator(): could not find a curve element with t <= u!");
        return p;
    }

    if (i < 3 || i > myPrivate->controlPoints.getSize() + 3)
    {
        goLog::warning ("goNURBS::operator(): i < 3 or i > num. controlPoints + 3");
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
* @brief Interpolate with given point list.
*
* Copies <code>points</code> to the internal point list,
* adds the last point twice
* and calls interpolate().
* 
* @param points  The point list describing the curve to be approximated.
*
* @return True if successful, false otherwise.
**/
bool goNURBS::interpolate (const goList<goPointf>& points)
{
    if (points.getSize() < 4)
        return false;

    myPrivate->controlPoints.erase();
    const goPointf* p = &points.getFrontElement()->elem;
    assert (p);
    goPointf pp = *p;
    pp.w = 1.0f;
    goIndex_t i = 0;
    goList<goPointf>::ConstElement* el = points.getFrontElement();
    while (true)
    {
        pp = el->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
        if (!el->next)
            break;
        el = el->next;
        ++i;
    }
    pp = points.getTailElement()->elem;
    pp.w = 1.0f;
    myPrivate->controlPoints.append(pp);
    this->interpolate ();
    return true;
}

bool goNURBS::interpolate (const goList<goPointd>& points)
{
    if (points.getSize() < 4)
        return false;

    myPrivate->controlPoints.erase();
    const goPointd* p = &points.getFrontElement()->elem;
    assert (p);
    goPointf pp;
    pp.w = 1.0f;
    goIndex_t i = 0;
    goList<goPointd>::ConstElement* el = points.getFrontElement();
    while (true)
    {
        pp = el->elem;
        pp.w = 1.0f;
        myPrivate->controlPoints.append(pp);
        if (!el->next)
            break;
        el = el->next;
        ++i;
    }
    pp = points.getTailElement()->elem;
    pp.w = 1.0f;
    myPrivate->controlPoints.append(pp);
    this->interpolate ();
    return true;
}
