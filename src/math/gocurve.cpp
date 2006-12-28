#include <gocurve.h>
#include <gopoint.h>
#include <golist.h>
#include <gonubs.h>
#include <goconfig.h>
#include <gomatrix.h>
#include <gofilter1d.h>

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
        goCurvePrivate() {}; 
        ~goCurvePrivate() {};
};

// =====================================

template<class T>
goCurve<T>::goCurve (goSize_t dim)
    : goPointCloud<T> (dim),
      myPrivate (0)
{
    this->setClassID(GO_CURVE);
    myPrivate = new goCurvePrivate;
    assert (myPrivate);
}

template<class T>
goCurve<T>::goCurve (const goCurve<T>& other)
    : goPointCloud<T> (other),
      myPrivate (0)
{
    myPrivate = new goCurvePrivate;
    assert (myPrivate);
    *this = other;
}

template<class T>
goCurve<T>::goCurve (const goList<goVector<T> >& pl)
    : goPointCloud<T> (pl),
      myPrivate (0)
{
    myPrivate = new goCurvePrivate;
    assert (myPrivate);
}

template<class T>
goCurve<T>& goCurve<T>::operator= (const goCurve<T>& other)
{
    *myPrivate = *other.myPrivate;
    goPointCloud<T>::operator= (other);
    return *this;
}

template<class T>
goCurve<T>::~goCurve ()
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
template <class T>
bool goCurve<T>::setPoints (const goList<goVector<T> >& l)
{
    return goPointCloud<T>::setPoints(l);
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
template <class T>
bool goCurve<T>::setPoints (typename goList<goVector<T> >::ConstElement* sourceBegin, 
                            goIndex_t                         sourcePointCount, 
                            goIndex_t                         destPointCount,
                            bool                              closed)
{
    goList<goVector<T> >& points = this->getPoints();
    points.erase();
    bool result = goCurve<T>::resample(sourceBegin, sourcePointCount, destPointCount, points, closed);
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
template <class T>
bool goCurve<T>::setPoints (typename goList<goVector<T> >::ConstElement* sourceBegin, goIndex_t sourcePointCount, bool closed)
{
    goList<goVector<T> >& points = this->getPoints();
    points.erase();
    goIndex_t i;
    typename goList<goVector<T> >::ConstElement* el = sourceBegin;
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
template <class T>
goDouble goCurve<T>::euclideanDistance (const goCurve<T>& other, bool forward) const
{
    goIndex_t pointCount = this->getPointCount();
    if (other.getPointCount() != pointCount)
    {
        return -1.0;
    }
    assert (other.getPointCount() == this->getPointCount());
  
    //= Calculate square sum of point distances.
    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    typename goList<goVector<T> >::ConstElement* otherEl = 0;
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
//    goFloat temp1 = 0.0f;
//    goFloat temp2 = 0.0f;
    assert (el && otherEl);
    while (el && otherEl && i < pointCount)
    {
//        temp1 = el->elem.x - otherEl->elem.x;
//        ++i;
//        temp2 = el->elem.y - otherEl->elem.y;
        ++i;
        sum += (el->elem - otherEl->elem).abs (); // sqrt(temp1 * temp1 + temp2 * temp2);
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
#if 0
template<class T>
bool goCurve<T>::resampleNUBS (goIndex_t pointCount, goCurve<T>& ret) const
{
    if (pointCount <= 1)
        return false;

    goNUBS nubs (this);
    goList<goVector<T> > newPoints;
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
#endif

template<class T>
bool goCurve<T>::resample (goIndex_t pointCount, goList<goVector<T> >& ret) const
{
    goIndex_t np = this->getPoints().getSize();
    bool ok = goCurve<T>::resample (this->getPoints().getFrontElement(), np, pointCount, ret, this->getPoints().isClosed());
    if (ok && this->getPoints().isClosed())
    {
        ret.close();
    }
    return ok;
}

template<class T>
bool goCurve<T>::resample (goIndex_t pointCount, goCurve<T>& ret) const
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
#if 0
template<class T>
bool goCurve<T>::resampleNUBS (typename goList<goVector<T> >::ConstElement* begin, typename goList<goVector<T> >::ConstElement* end, goIndex_t pointCount, goList<goVector<T> >& ret)
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
#endif

template<class T>
bool goCurve<T>::resample (typename goList<goVector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goVector<T> >& ret, bool closedCurve)
{
    return goCurve<T>::resampleLinear (begin,pointCount,resamplePointCount,ret,closedCurve);
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
#if 0
template<class pointT>
bool goCurve<pointT>::resampleNUBS (typename goList<goVector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goVector<T> >& ret)
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
#endif

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
template<class T>
bool goCurve<T>::resampleLinear (typename goList<goVector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goVector<T> >& ret, bool closedCurve)
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
        typename goList<goVector<T> >::ConstElement* el = begin;
        goIndex_t i;
        for (i = 1; i < pointCount; ++i, el = el->next)
        {
            goVector<T> p = el->elem - el->next->elem;
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
    typename goList<goVector<T> >::ConstElement* el = begin;
    for (i = 0; i < resamplePointCount; ++i)
    {
        assert (el && el->next);
        assert (j >= 0 && j < pointCount - 1);
        goVector<T> p;
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
* Silently assumes dimension is 2.
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
template <class T>
bool goCurve<T>::readASCII (FILE* f, goList<goVector<T> >& ret)
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
            throw (goFileIOException(goFileIOException::UNEXPECTED_DATA));
            return false;
        }
        goList<goString>::Element* el = words.getFrontElement();
        if (el->elem != "curve")
        {
            goString msg = "goCurve::readASCII(): expected 'curve ...', got '";
            msg += line;
            msg += "'";
            goLog::warning(msg);
            throw (goFileIOException(goFileIOException::UNEXPECTED_DATA));
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

    goSize_t dim = 0;

    if (!goFileIO::readASCIILine (f, line))
    {
        return false;
    }
    {
        goList<goString> words;
        line.getWords(words);
        if (words.getSize() < 2)
        {
            goString msg = "goCurve::readASCII(): expected dimensionality, got '";
            msg += line;
            msg += "'";
            goLog::warning(msg);
            throw (goFileIOException(goFileIOException::UNEXPECTED_DATA));
            return false;
        }
        goList<goString>::Element* el = words.getFrontElement();
        if (el->elem != "dimension")
        {
            goString msg = "goCurve::readASCII(): expected 'dimension ...', got '";
            msg += line;
            msg += "'";
            goLog::warning(msg);
            throw (goFileIOException(goFileIOException::UNEXPECTED_DATA));
            return false;
        }

        dim = el->next->elem.toInt();
    }
    unsigned int pointCount = 0;
    fscanf(f,"%d\n",&pointCount);
    unsigned int i;
    goVector<T> p(dim);
    float x = 0.0f;
    // assert (p.getSize() >= 2);
    for (i = 0; i < pointCount; ++i)
    {
        goSize_t j;
        for (j = 0; j < dim - 1; ++j)
        {
            if (fscanf(f,"%f ",&x) < 1)
            {
                goLog::warning("goCurve::readASCII(): could not read point from a line.");
                throw (goFileIOException(goFileIOException::UNEXPECTED_DATA));
                return false;
            }
            p[j] = x;
        }
        if (fscanf(f,"%f\n",&x) < 1)
        {
            goLog::warning("goCurve::readASCII(): could not read point from a line.");
            throw (goFileIOException(goFileIOException::UNEXPECTED_DATA));
            return false;
        }
        p[dim-1] = x;
        ret.append(p);
    }
    if (closed)
    {
        ret.close();
    }
    goCurve<T>::removeDuplicates (ret);
    return true;
}

/** 
 * @brief Write point list in ASCII.
 *
 * Like readASCII(), assumes pointT is of size 2 at least (e.g. goPoint).
 * This is sufficient so far, but extend if needed.
 * 
 * @param f 
 * @param pointList 
 * 
 * @return 
 */
template <class T>
bool goCurve<T>::writeASCII (FILE* f, const goList<goVector<T> >& pointList)
{
    if (pointList.getSize() < 1)
    {
        return true;
    }
    if (!f)
    {
        return false;
    }
    if (!goFileIO::writeASCII (f, goString("curve")))
    {
        return false;
    }
    if (pointList.isClosed())
    {
        if (!goFileIO::writeASCII (f, goString(" closed")))
        {
            return false;
        }
    }
    goFileIO::writeASCII (f, goString("\n"));
    goString dimString = "dimension ";
    goSize_t dim = 0;
    goSize_t pointCount = pointList.getSize();
    if (pointCount > 0)
    {
        dim = pointList.getFrontElement()->elem.getSize();
        dimString += static_cast<int> (dim);
    }
    else
    {
        dimString += "0";
    }
    dimString += "\n";
    goFileIO::writeASCII (f, dimString);
    goString line = "";
    line += (int)pointCount;
    line += "\n";
    if (!goFileIO::writeASCII (f, line))
    {
        return false;
    }
    typename goList<goVector<T> >::ConstElement* el = pointList.getFrontElement();
    goSize_t i;
    for (i = 0; i < pointCount; ++i, el = el->next)
    {
        assert(el);
        goSize_t j;
        line = "";
        for (j = 0; j < dim - 1; ++j)
        {
            line += (float)el->elem[j];
            line += " ";
        }
        line += (float)el->elem[dim-1];
        line += "\n";
        if (!goFileIO::writeASCII (f, line))
        {
            return false;
        }
    }
    return true;
}

/** 
 * @brief Read from a simple ASCII file (like for gnuplot)
 * 
 * Each line starting with # is treated as comment as in goPointCloud<T>::readASCII().
 *
 * @param filename  Name of the file.
 * @param dimension Dimension of the points (i.e. numbers per line).
 *                  This may be less than the number of entries per line actually in the file,
 *                  but it may not be larger.
 * @param closed    If true, the point list will be closed.
 * 
 * @return True if successful, false otherwise. Also check the log file.
 */
template <class T>
bool goCurve<T>::readASCIISimple (const char* filename, goSize_t dimension, bool closed)
{
    this->getPoints().erase ();
    bool ok = goPointCloud<T>::readASCII (filename, dimension, this->getPoints());
    if (ok && closed)
    {
        this->getPoints().close ();
    }
    return ok;
}

/** 
 * @brief Removes duplicate entries in the point list.
 * 
 * @param pl Point list.
 * 
 * @return Total number of removed entries.
 */
template <class T>
goSize_t goCurve<T>::removeDuplicates (goList<goVector<T> >& pl)
{
    goSize_t removed = 0;
    goSize_t removed_total = 0;
    do
    {
        goSize_t i = 0;
        removed = 0;
        goSize_t sz = pl.getSize();
        typename goList<goVector<T> >::Element* el = pl.getFrontElement();
        if (!pl.isClosed())
        {
            --sz;
        }
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
        removed_total += removed;
    } while (removed > 0 );
    return removed_total;
}

/** 
 * @brief Filter the curve points with a linear filter mask.
 * 
 * @param mask Mask.
 * @param size Size of the mask.
 * @param center Center index of the mask.
 * @param pointList The points.
 * @param count The number of times to apply the filter mask (default: 1)
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goCurve<T>::filter (const goFloat* mask, goSize_t size, goSize_t center, goList<goVector<T> >& pointList, goSize_t count)
{
    if (pointList.isEmpty())
        return true;

    goMatrix<T> M (pointList.getFront().getSize(), pointList.getSize());
    typename goList<goVector<T> >::Element* el = pointList.getFrontElement();
    goSize_t sz = pointList.getSize();
    goVector<T> column;
    for (goSize_t i = 0; i < sz; ++i)
    {
        M.refColumn (i, column);
        column = el->elem;      //= Works because column is of same size as el->elem.
        el = el->next;
    }

    goFilter1D filter (mask, size, center);
    for (goSize_t i = 0; i < count; ++i)
        filter.filter (M, 0);

    el = pointList.getFrontElement();
    for (goSize_t i = 0; i < sz; ++i)
    {
        M.refColumn (i, column);
        el->elem = column;      //= Works because column is of same size as el->elem.
        el = el->next;
    }

    return true;
}

template <class T>
goSize_t goCurve<T>::removeDuplicates ()
{
    return goCurve<T>::removeDuplicates (this->getPoints());
}

template <class T>
bool goCurve<T>::writeASCII (FILE* f) const
{
    return goCurve<T>::writeASCII (f, this->getPoints());
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
template <class T>
bool goCurve<T>::readASCII (FILE* f)
{
    bool ok = false;
    try
    {
        ok = goCurve<T>::readASCII (f, this->getPoints());
    }
    catch (goFileIOException& ex)
    {
        throw (ex);
    }
    return ok;
}

template<class T>
bool goCurve<T>::getGradNorm (goArray<goFloat>& diffNorm) const
{
    if (this->getPoints().isEmpty())
        return false;

    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    
    const goVector<T>* p1 = 0;
    const goVector<T>* p2 = 0;
//    goFloat temp1 = 0.0f;
//    goFloat temp2 = 0.0f;
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
//        temp1 = p2->x - p1->x;
//        temp2 = p2->y - p1->y;
        diffNorm[i] = (*p2 - *p1).abs(); // sqrt(temp1*temp1+temp2*temp2);
        if (!el->next)
            break;
        el = el->next;
        ++i;
    }
    return true;
}

template<class T>
bool goCurve<T>::getGrad (goList<goVector<T> >& diff) const
{
    if (this->getPoints().isEmpty())
        return false;
    
    diff.erase();
    goVector<T> d(this->getDim());
    d.fill (T(0));
    
    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    assert (el);
    const goVector<T>* p1 = 0;
    const goVector<T>* p2 = 0;
    goSize_t pointCount = this->getPointCount ();
    goSize_t j = 0;
    while (el && j < pointCount)
    {
        p1 = &el->elem;
        if (el->next)
            p2 = &el->next->elem;
        else
            p2 = p1;
        d = p2 - p1;
        // d.x = p2->x - p1->x;
        // d.y = p2->y - p1->y;
        diff.append (d);
        el = el->next;
        ++j;
    }
    return true;
}

template<class T>
bool goCurve<T>::getCurvNorm (goArray<goFloat>& curvNorm) const
{
    if (this->getPoints().isEmpty())
        return false;

    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    
    const goVector<T>* p1 = 0;
    const goVector<T>* p  = 0;
    const goVector<T>* p2 = 0;
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
        temp1 = 0.5*(*p2)[0] - (*p)[0] + 0.5*(*p1)[0];
        temp2 = 0.5*(*p2)[1] - (*p)[1] + 0.5*(*p1)[1];
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
#if 0
template<class T>
bool goCurve<T>::getAngleFunction (goArray<goFloat>& angles, const goVector<T>& axis) const
{
    if (this->getPoints().isEmpty())
        return false;
    angles.resize (this->getPoints().getSize());
    goVector<T> a = axis;
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
    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
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
        delta[0] = (*p2)[0] - (*p1)[0];
        delta[1] = (*p2)[1] - (*p1)[1];
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
#endif

template <class T>
static goDouble getTurn (const goVector<T>& p1, const goVector<T>& p2, const goVector<T>& p3)
{
    goVector<T> base = p3 - p1;
    goDouble f = base.abs();
    if (f == 0.0)
    {
        assert ("p3 == p1" == 0);
        return 0.0;  //= This should not happen. It would mean p3 == p1.
    }
    base *= 1.0 / f;
    goVector<T> s1 = p2 - p1;
    goVector<T> s2 = p3 - p2;
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
    return beta * ((s1[0] * s2[1] < s1[1] * s2[0]) ? -1.0 : 1.0);
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
template <class T>
bool goCurve<T>::getTurningFunction (goVector<T>& ret) const
{
    goIndex_t sz = this->getPoints().getSize();

    if (sz <= 0)
        return false;

    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
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
template< class T>
goDouble goCurve<T>::getLength () const
{
    if (this->getPoints().isEmpty())
        return 0.0;

    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    assert (el);
    goIndex_t count = this->getPoints().getSize();
    goDouble length = 0.0;
    goVector<T> lastPoint = el->elem;
    goVector<T> d;
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

template< class T>
goDouble goCurve<T>::getLength (const goList<goVector<T> >& pl) 
{
    if (pl.isEmpty())
        return 0.0;

    typename goList<goVector<T> >::ConstElement* el = pl.getFrontElement();
    assert (el);
    goIndex_t count = pl.getSize();
    goDouble length = 0.0;
    goVector<T> lastPoint = el->elem;
    goVector<T> d;
    while (count > 0 && el)
    {
        d = el->elem - lastPoint;
        length += d.abs();
        lastPoint = el->elem;
        el = el->next;
        --count;
    }
    if (pl.isClosed())
    {
        d = pl.getFrontElement()->elem - pl.getTailElement()->elem;
        length += d.abs();
    }
    return length;
}


template<class T>
bool goCurve<T>::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    return goObjectBase::callObjectMethod (methodID, param);
}

template<class T>
void goCurve<T>::receiveObjectMessage (const goObjectMessage& msg)
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
template<class T>
bool goCurve<T>::writeObjectFile (FILE* f) const
{
    if (!f)
        return false;
    if (!goFileIO::writeASCII (f, goString("goCurve")))
    {
        return false;
    }
    const char cnull = 0;
    fwrite (&cnull, sizeof(char), 1, f);
    return goPointCloud<T>::writeObjectFile (f);
}

/**
* @brief Read an object from a file as goCurve.
*
* @param f  Valid, open file.
*
* @return True if successful, false otherwise.
**/
template<class T>
bool goCurve<T>::readObjectFile  (FILE* f)
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
    return goPointCloud<T>::readObjectFile (f);
}

template class goCurve <goFloat>;
template class goCurve <goDouble>;
