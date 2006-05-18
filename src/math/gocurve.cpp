#include <gocurve.h>
#include <gopoint.h>
#include <golist.h>
#include <gonubs.h>
#include <goconfig.h>

#ifndef GOLOG_H
# include <golog.h>
#endif
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif

#include <assert.h>
#include <math.h>

class goCurvePrivate
{
    public:
        goCurvePrivate();
        ~goCurvePrivate();
};

goCurvePrivate::goCurvePrivate ()
{
}

goCurvePrivate::~goCurvePrivate ()
{
}

// =====================================

template<class pointT>
goCurve<pointT>::goCurve ()
    : goPointCloud<pointT> (),
      myPrivate (0)
{
    this->setClassName ("goCurve");
    myPrivate = new goCurvePrivate;
    assert (myPrivate);
}

template<class pointT>
goCurve<pointT>::goCurve (const goCurve<pointT>& other)
    : goPointCloud<pointT> (other),
      myPrivate (0)
{
    myPrivate = new goCurvePrivate;
    assert (myPrivate);
    *this = other;
}

template<class pointT>
goCurve<pointT>::goCurve (const goList<pointT>& pl)
    : goPointCloud<pointT> (pl),
      myPrivate (0)
{
    myPrivate = new goCurvePrivate;
    assert (myPrivate);
}

template<class pointT>
goCurve<pointT>& goCurve<pointT>::operator= (const goCurve<pointT>& other)
{
    *myPrivate = *other.myPrivate;
    goPointCloud<pointT>::operator= (other);
    return *this;
}

template<class pointT>
goCurve<pointT>::~goCurve ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

// Defined in pointcloud
#if 0
template <class pointT>
bool goCurve<pointT>::operator== (const goCurve<pointT>& other) const
{
    return this->getPoints() == other.getPoints();
}

template <class pointT>
bool goCurve<pointT>::operator!= (const goCurve<pointT>& other) const
{
    return !(*this == other);
}
#endif

/**
 * @bug Check this -- this can't be the right solution.
 * If this is not present, setPoints() from goPointCloud can not be used because of the overloaded versions in goCurve.
 */
template <class pointT>
bool goCurve<pointT>::setPoints (const goList<pointT>& l)
{
    return goPointCloud<pointT>::setPoints(l);
}

/** 
 * @brief Resamples new points uniformly from given source points and sets the
 *        resampled points to this curve.
 * 
 * @param sourceBegin       First list element of source points.
 * @param sourcePointCount  Number of source points.
 * @param destPointCount    Number of points you want in this goCurve.
 * @param closed            If true, assume the source and destination shall be closed. If false, they are open.
 * 
 * @return True if successful, false otherwise.
 */
template <class pointT>
bool goCurve<pointT>::setPoints (typename goList<pointT>::ConstElement* sourceBegin, 
                                 goIndex_t                              sourcePointCount, 
                                 goIndex_t                              destPointCount,
                                 bool                                   closed)
{
    goList<pointT>& points = this->getPoints();
    points.erase();
    bool result = goCurve<pointT>::resample(sourceBegin, sourcePointCount, destPointCount, points, closed);
    if (!result)
    {
        return false;
    }
    if (closed)
    {
        points.close();
    }
    return true;
}

/** 
 * @brief Set points from source list.
 * 
 * @param sourceBegin       First element of a point list.
 * @param sourcePointCount  Number of points to copy.
 * @param closed            If true, this curve's point list will be closed.
 * 
 * @return True if successful, false otherwise.
 */
template <class pointT>
bool goCurve<pointT>::setPoints (typename goList<pointT>::ConstElement* sourceBegin, goIndex_t sourcePointCount, bool closed)
{
    goList<pointT>& points = this->getPoints();
    points.erase();
    goIndex_t i;
    typename goList<pointT>::ConstElement* el = sourceBegin;
    for (i = 0; i < sourcePointCount && el; ++i)
    {
        points.append(el->elem);
        el = el->next;
    }
    if (closed)
    {
        points.close();
    }
    return true;
}

/** 
 * @brief Calculate euclidean distance between two curves.
 * 
 * No transformations are applied to either curve, except reversal if the 
 * forward parameter is false.
 * The distance is simply the sum of the distances between each point pair of
 * the two curves.
 * 
 * @param other   Other curve. Must have same point count as this curve.
 * @param forward If false, the other curve will be reversed when calculating (the object itself 
 *                ist not altered). Default is true.
 * 
 * @return Distance between this and other. If negative, an error occured (probably unequal point counts).
 */
template <class pointT>
goDouble goCurve<pointT>::euclideanDistance (const goCurve<pointT>& other, bool forward) const
{
    goIndex_t pointCount = this->getPointCount();
    if (other.getPointCount() != pointCount)
    {
        return -1.0;
    }
    assert (other.getPointCount() == this->getPointCount());
  
    //= Calculate square sum of point distances.
    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    typename goList<pointT>::ConstElement* otherEl = 0;
    if (forward)
    {
        otherEl = other.getPoints().getFrontElement();
    }
    else
    {
        otherEl = other.getPoints().getTailElement();
    }
    goIndex_t i   = 0;
    goDouble sum  = 0.0;
    goFloat temp1 = 0.0f;
    goFloat temp2 = 0.0f;
    assert (el && otherEl);
    while (el && otherEl && i < pointCount)
    {
        temp1 = el->elem.x - otherEl->elem.x;
        ++i;
        temp2 = el->elem.y - otherEl->elem.y;
        ++i;
        sum += sqrt(temp1 * temp1 + temp2 * temp2);
        el = el->next;
        if (forward)
        {
            otherEl = otherEl->next;
        }
        else
        {
            otherEl = otherEl->prev;
        }
    }
    return sum;
}

/**
 * @brief Uniformly resamples the curve with the given number of points.
 *
 * @note This uses approximating splines. The original points are not interpolated!
 * 
 * @param pointCount  Number of points. Must be > 1.
 * @param ret         Resampled curve.
 *
 * @return True if successful, false otherwise.
 **/
template<class pointT>
bool goCurve<pointT>::resampleNUBS (goIndex_t pointCount, goCurve<pointT>& ret) const
{
    if (pointCount <= 1)
        return false;

    goNUBS nubs (this);
    goList<pointT> newPoints;
    goFloat t = 0.0f;
    goFloat step = nubs.getCurveLength() / (float)(pointCount-1);
    goIndex_t i;
    pointT p;
    for (i = 0; i < pointCount - 1; ++i, t += step)
    {
        p = nubs (t);
        newPoints.append (p);
    }
    p = nubs (t - 1e-5);
    newPoints.append(p);
    if (this->getPoints().isClosed())
    {
        newPoints.close();
    }
    ret.setPoints (newPoints);
    return true;
}

template<class pointT>
bool goCurve<pointT>::resample (goIndex_t pointCount, goList<pointT>& ret) const
{
    goIndex_t np = this->getPoints().getSize();
    bool ok = goCurve<pointT>::resample (this->getPoints().getFrontElement(), np, pointCount, ret, this->getPoints().isClosed());
    if (ok && this->getPoints().isClosed())
    {
        ret.close();
    }
    return ok;
}

template<class pointT>
bool goCurve<pointT>::resample (goIndex_t pointCount, goCurve<pointT>& ret) const
{
    return this->resample (pointCount, ret.getPoints());
}

/** 
 * @brief 
 * 
 * @note This uses approximating splines. The original points are not interpolated!
 * 
 * @param begin 
 * @param end 
 * @param pointCount 
 * @param ret 
 * 
 * @return 
 */
template<class pointT>
bool goCurve<pointT>::resampleNUBS (typename goList<pointT>::ConstElement* begin, typename goList<pointT>::ConstElement* end, goIndex_t pointCount, goList<pointT>& ret)
{
    goNUBS nubs;
    if (!nubs.setControlPoints(begin,end))
        return false;
    goFloat t = 0.0f;
    goFloat step = nubs.getCurveLength() / (float)(pointCount-1);
    goIndex_t i;
    pointT p;
    for (i = 0; i < pointCount - 1; ++i, t += step)
    {
        p = nubs (t);
        ret.append (p);
    }
    p = nubs(t - 1e-5);
    ret.append(p);
    return true;
}

template<class pointT>
bool goCurve<pointT>::resample (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<pointT>& ret, bool closedCurve)
{
    return goCurve<pointT>::resampleLinear (begin,pointCount,resamplePointCount,ret,closedCurve);
}

/** 
 * @brief 
 *
 * @note This uses approximating splines. The original points are not interpolated!
 * 
 * @param begin 
 * @param pointCount 
 * @param resamplePointCount 
 * @param ret 
 * 
 * @return 
 */
template<class pointT>
bool goCurve<pointT>::resampleNUBS (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<pointT>& ret)
{
    goNUBS nubs;
    if (!nubs.setControlPoints(begin,pointCount))
        return false;
    goDouble t = 0.0f;
    goDouble step = nubs.getCurveLength() / (float)(resamplePointCount-1);
    goIndex_t i;
    pointT p;
    for (i = 0; i < resamplePointCount - 1; ++i, t += step)
    {
        p = nubs (t);
        ret.append (p);
    }
    p = nubs(t - 1e-5);
    ret.append(p);
    return true;
}

/** 
 * @brief 
 *
 * @note This uses approximating splines. The original points are not interpolated!
 * 
 * @param begin 
 * @param pointCount 
 * @param resamplePointCount 
 * @param ret 
 * 
 * @return 
 */
template<class pointT>
bool goCurve<pointT>::resampleLinear (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<pointT>& ret, bool closedCurve)
{
    if (pointCount < 2)
    {
        goLog::warning("goCurve::resampleLinear(): point count is < 2.");
        return false;
    }
    if (resamplePointCount < 2)
    {
        return false;
    }
    goDouble curveLength = 0.0;

    //= If the curve is closed, the point list must be closed (or contain one more point)
    //= and we must resample from the last to the first point too.
    if (closedCurve)
    {
        ++pointCount;
    }
    
    goFixedArray<goDouble> accumLength (pointCount);
    
    {
        accumLength[0] = 0.0;
        typename goList<pointT>::ConstElement* el = begin;
        goIndex_t i;
        for (i = 1; i < pointCount; ++i, el = el->next)
        {
            pointT p = el->elem - el->next->elem;
            curveLength += p.abs();
            accumLength[i] = curveLength;
        }
    }

    goDouble step = 0.0;
    if (closedCurve)
    {
        step = curveLength / (double)(resamplePointCount);
    }
    else
    {
        step = curveLength / (double)(resamplePointCount-1);
    }
    goDouble t = 0.0f;
    goIndex_t i = 0;
    goIndex_t j = 0;
    pointT p;
    typename goList<pointT>::ConstElement* el = begin;
    for (i = 0; i < resamplePointCount; ++i)
    {
        assert (el && el->next);
        assert (j >= 0 && j < pointCount - 1);
        pointT p;
        goDouble e = (t - accumLength[j]) / (accumLength[j+1] - accumLength[j]);
        p = el->next->elem * e + el->elem * (1-e);
        ret.append (p);
        t += step;
        while (j < pointCount - 2 && t > accumLength[j+1])
        {
            ++j;
            assert (el);
            el = el->next;
        }
    }
#if 0
    {
        if (j >= pointCount)
        {
            j = pointCount - 1;
        }
        goDouble e = ((t - 1e-5) - accumLength[j-1]) / (accumLength[j] - accumLength[j-1]);
        pointT p;
        p = el->elem * e + el->prev->elem * (1-e);
        ret.append (p);
    }
#endif
    return true;
}

/** 
* @brief Read curve points from an ASCII C file stream.
* 
* The curve points are expected in this format:
* 
* ...
* curve\\n
* <integer: number of points>\\n
* <float: x coordinate point 1> <float: y coordinate point 1>\\n
* ....
* 
* 
* @param f C stream pointer.
* @param ret Contains points after method returns with true.
* 
* @return True if successful, false otherwise. Check log.
*/
template <class pointT>
bool goCurve<pointT>::readASCII (FILE* f, goList<pointT>& ret)
{
    if (!f)
    {
        return false;
    }

    bool closed = false;
    
    goString line;
    if (!goFileIO::readASCIILine (f, line))
    {
        return false;
    }
    {
        goList<goString> words;
        line.getWords(words);
        if (words.getSize() < 1)
        {
            goString msg = "goCurve::readASCII(): expected 'curve ...', got '";
            msg += line;
            msg += "'";
            goLog::warning(msg);
            return false;
        }
        goList<goString>::Element* el = words.getFrontElement();
        if (el->elem != "curve")
        {
            goString msg = "goCurve::readASCII(): expected 'curve ...', got '";
            msg += line;
            msg += "'";
            goLog::warning(msg);
            return false;
        }
        while (el)
        {
            if (el->elem == "closed")
            {
                closed = true;
            }
            //= ... add more keywords here as needed
            el = el->next;
        }
    }
    unsigned int pointCount = 0;
    fscanf(f,"%d\n",&pointCount);
    unsigned int i;
    pointT p;
    float x = 0.0f;
    float y = 0.0f;
    for (i = 0; i < pointCount; ++i)
    {
        if (fscanf(f,"%f %f\n",&x,&y) < 2)
        {
            goLog::warning("goCurve::readASCII(): could not read 2 points from a line.");
            return false;
        }
        p.x = x; p.y = y;
        ret.append(p);
    }
    if (closed)
    {
        ret.close();
    }
    goCurve<pointT>::removeDuplicates (ret);
    return true;
}

template <class pointT>
bool goCurve<pointT>::writeASCII (FILE* f, const goList<pointT>& pointList)
{
    if (pointList.getSize() < 1)
    {
        return true;
    }
    if (!f)
    {
        return false;
    }
    if (!goFileIO::writeASCII (f, goString("curve\n")))
    {
        return false;
    }
    goSize_t pointCount = pointList.getSize();
    goString line = "";
    line += (int)pointCount;
    line += "\n";
    if (!goFileIO::writeASCII (f, line))
    {
        return false;
    }
    typename goList<pointT>::ConstElement* el = pointList.getFrontElement();
    goSize_t i;
    for (i = 0; i < pointCount; ++i, el = el->next)
    {
        assert(el);
        line = "";
        line += (float)el->elem.x;
        line += " ";
        line += (float)el->elem.y;
        line += "\n";
        if (!goFileIO::writeASCII (f, line))
        {
            return false;
        }
    }
    return true;
}

template <class pointT>
goSize_t goCurve<pointT>::removeDuplicates (goList<pointT>& pl)
{
    typename goList<pointT>::Element* el = pl.getFrontElement();
    goSize_t sz = pl.getSize();
    if (!pl.isClosed())
    {
        --sz;
    }
    goSize_t i = 0;
    goSize_t removed = 0;
    while (i < sz && el)
    {
        if (el->elem == el->next->elem)
        {
            el = pl.remove(el);
            ++removed;
        }
        else
        {
            el = el->next;
        }
        ++i;
    }
    return removed;
}

template <class pointT>
goSize_t goCurve<pointT>::removeDuplicates ()
{
    return goCurve<pointT>::removeDuplicates (this->getPoints());
}

template <class pointT>
bool goCurve<pointT>::writeASCII (FILE* f) const
{
    return goCurve<pointT>::writeASCII (f, this->getPoints());
}

/** 
* @brief Read curve points from ASCII C file stream into this curve.
* 
* @see Static member readASCII.
* 
* @param f C file stream.
* 
* @return True if successful, false otherwise.
*/
template <class pointT>
bool goCurve<pointT>::readASCII (FILE* f)
{
    return goCurve<pointT>::readASCII (f, this->getPoints());
}

template<class pointT>
bool goCurve<pointT>::getGradNorm (goArray<goFloat>& diffNorm) const
{
    if (this->getPoints().isEmpty())
        return false;

    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    
    const pointT* p1 = 0;
    const pointT* p2 = 0;
    goFloat temp1 = 0.0f;
    goFloat temp2 = 0.0f;
    diffNorm.resize (this->getPoints().getSize());
    goIndex_t i = 0;
    goIndex_t size = diffNorm.getSize();

    while (true && i < size)
    {
        p1 = &el->elem;
        if (el->next)
            p2 = &el->next->elem;
        else
            p2 = p1;
        temp1 = p2->x - p1->x;
        temp2 = p2->y - p1->y;
        diffNorm[i] = sqrt(temp1*temp1+temp2*temp2);
        if (!el->next)
            break;
        el = el->next;
        ++i;
    }
    return true;
}

template<class pointT>
bool goCurve<pointT>::getGrad (goList<go4Vectorf>& diff) const
{
    if (this->getPoints().isEmpty())
        return false;
    
    diff.erase();
    go4Vectorf d (0.0f, 0.0f, 0.0f, 0.0f);
    
    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    assert (el);
    const pointT* p1 = 0;
    const pointT* p2 = 0;
    while (true)
    {
        p1 = &el->elem;
        if (el->next)
            p2 = &el->next->elem;
        else
            p2 = p1;
        d.x = p2->x - p1->x;
        d.y = p2->y - p1->y;
        diff.append (d);
        if (!el->next)
            break;
        el = el->next;
    }
    return true;
}

template<class pointT>
bool goCurve<pointT>::getCurvNorm (goArray<goFloat>& curvNorm) const
{
    if (this->getPoints().isEmpty())
        return false;

    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    
    const pointT* p1 = 0;
    const pointT* p  = 0;
    const pointT* p2 = 0;
    goFloat temp1 = 0.0f;
    goFloat temp2 = 0.0f;
    curvNorm.resize (this->getPoints().getSize());
    goIndex_t i = 0;
    goIndex_t size = curvNorm.getSize();

    // FIXME: Calculate the real gradient norm here or the curvature.
    while (true && i < size)
    {
        p = &el->elem;
        if (el->prev)
            p1 = &el->prev->elem;
        else
            p1 = p;
        if (el->next)
            p2 = &el->next->elem;
        else
            p2 = p;
        temp1 = 0.5*p2->x - p->x + 0.5*p1->x;
        temp2 = 0.5*p2->y - p->y + 0.5*p1->y;
        curvNorm[i] = sqrt(temp1*temp1+temp2*temp2);
        if (!el->next)
            break;
        el = el->next;
        ++i;
    }
    return true;
}

/**
 * @brief Calculate the angle function for a 2D-curve.
 *
 * @param angles  Contains the angles at each curve point after the method returned true.
 * @param axis    The axis against which to measure the angles.
 *
 * @return True if successful, false otherwise.
 **/
template<class pointT>
bool goCurve<pointT>::getAngleFunction (goArray<goFloat>& angles, const go4Vectorf& axis) const
{
    if (this->getPoints().isEmpty())
        return false;
    angles.resize (this->getPoints().getSize());
    go4Vectorf a = axis;
    goFloat a_abs = a.abs();
    if (a_abs == 0.0f)
    {
        return false;
    }
    a *= 1.0f / a_abs;
    //= Find a perpendicular vector to the axis.
    go4Vectorf b (1.0f, 0.0f, 0.0f, 0.0f);
    go4Vectorf a_ = a;
    if (1.0f - fabs(a*b) < 1e-6)
    {
        b = go4Vectorf (0.0f, 1.0f, 0.0f, 0.0f);
    }
    a_.cross(b);
    a_.cross(a);
    b = a_;
    if (b.abs() == 0.0f)
        return false;
    b *= 1.0f / b.abs();

    goIndex_t i;
    goIndex_t size = angles.getSize();
    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    assert (el);
    const pointT* p1 = 0;
    const pointT* p2 = 0;
    go4Vectorf delta;
    goFloat delta_abs = 0.0f;
    for (i = 0; i < size; ++i)
    {
        p1 = &el->elem;
        if (el->next)
            p2 = &el->next->elem;
        else
            p2 = p1;
        delta.x = p2->x - p1->x;
        delta.y = p2->y - p1->y;
        delta_abs = delta.abs();
        if (delta_abs != 0.0f)
        {
            angles[i] = acos(a * delta / delta_abs);
            //= This would take care for the function to be between 0 and 2*PI, but
            //= it will have discontinuities then at 0/2*PI radians.
            // if (b * delta <= 0.0f)
            //    angles[i] = 2*M_PI - angles[i];
        }
        else
        {
            angles[i] = 0.0f;
        }
        if (el->next)
            el = el->next;
    }
    return true;
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

/**
* @brief Turning function.
*
* The turning function contains for each point p_i the angle between (p_{i-1},p_i) and
* (p_i,p_{i+1}). In case of non-closed curves, the first entry is defined to be zero, and
* there is one less entries than there are points.
* The turning function can be either positive or negative, depending on whether the corresponding
* point triple forms a triangle that turns clockwise or counterclockwise.
* 
* @return True if successful, false otherwise.
**/
template <class pointT>
bool goCurve<pointT>::getTurningFunction (goVectord& ret) const
{
    goIndex_t sz = this->getPoints().getSize();

    if (sz <= 0)
        return false;

    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    goIndex_t i = 0;

    if (!this->getPoints().isClosed())
    {
        if (el)
            el = el->next;
        sz -= 1;
        i = 1;
        ret.setSize (sz);
        ret[0] = 0.0;
    }
    else
    {
        ret.setSize (sz);
    }
    while (el && i < sz)
    {
        ret[i] = getTurn (el->prev->elem, el->elem, el->next->elem);
        ++i;
        el = el->next;
    }
    return true;
}

/**
 * @brief Calculate the length of the curve.
 *
 * @return The length of the curve, i.e. the sum of the line segments connecting the given points on the curve. 
 *         If the point list is closed, the curve is treated as closed and the line segment connecting
 *         the first and last point is added to the sum.
 **/
template< class pointT>
goDouble goCurve<pointT>::getLength () const
{
    if (this->getPoints().isEmpty())
        return 0.0;

    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    assert (el);
    goIndex_t count = this->getPoints().getSize();
    goDouble length = 0.0;
    pointT lastPoint = el->elem;
    pointT d;
    while (count > 0 && el)
    {
        d = el->elem - lastPoint;
        length += d.abs();
        lastPoint = el->elem;
        el = el->next;
        --count;
    }
    if (this->getPoints().isClosed())
    {
        d = this->getPoints().getFrontElement()->elem - this->getPoints().getTailElement()->elem;
        length += d.abs();
    }
    return length;
}

template<class pointT>
bool goCurve<pointT>::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    return goObjectBase::callObjectMethod (methodID, param);
}

template<class pointT>
void goCurve<pointT>::receiveObjectMessage (const goObjectMessage& msg)
{
    goObjectBase::receiveObjectMessage (msg);
}

/**
* @brief Write the object to a file as goCurve.
*
* @param f  Valid, open file.
*
* @return True if successful, false otherwise.
**/
template<class pointT>
bool goCurve<pointT>::writeObjectFile (FILE* f) const
{
    if (!f)
        return false;
    if (!goFileIO::writeASCII (f, goString("goCurve")))
    {
        return false;
    }
    const char cnull = 0;
    fwrite (&cnull, sizeof(char), 1, f);
    return goPointCloud<pointT>::writeObjectFile (f);
}

/**
* @brief Read an object from a file as goCurve.
*
* @param f  Valid, open file.
*
* @return True if successful, false otherwise.
**/
template<class pointT>
bool goCurve<pointT>::readObjectFile  (FILE* f)
{
    if (!f)
        return false;
    goString name;
    if (!goFileIO::readASCII (f, name))
    {
        return false;
    }
    if (name != "goCurve")
    {
        return false;
    }
    return goPointCloud<pointT>::readObjectFile (f);
}


template class goCurve <goPointf>;
template class goCurve <goPointd>;
