/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gocurve.h>
#include <gopoint.h>
#include <golist.h>
#include <gonubs.h>
#include <goconfig.h>
#include <gomatrix.h>
#include <gofilter1d.h>
#include <goresample.h>

#ifndef GOLOG_H
# include <golog.h>
#endif
#ifndef GOFILEIO_H
# include <gofileio.h>
#endif

#include <goexception.h>

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
goCurve<T>::goCurve (const goList<goMath::Vector<T> >& pl)
    : goPointCloud<T> (pl),
      myPrivate (0)
{
    myPrivate = new goCurvePrivate;
    assert (myPrivate);
}

template<class T>
goCurve<T>::goCurve (const goMath::Matrix<T>& confMatrix)
    : goPointCloud<T> (confMatrix),
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

/** 
 * @brief Sample a point from the curve polygon.
 *
 * Interpolates linearly to sample points from the polygon defined by
 * the points of this curve.
 *
 * This is quite slow since potentially all points are searched, 
 * but appears to work.
 * 
 * @param position Position on the curve in [0,this->getLength()]
 * @param ret      On return, contains the point at position.
 * 
 * @return True if successful, false otherwise (check logfile).
 */
template <class T>
bool goCurve<T>::sample (goDouble position, goMath::Vector<T>& ret) const
{
    if (this->getPoints().getSize() < 2)
    {
        return false;
    }
    goDouble pos1 = 0.0;

    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    goSize_t sz = this->getPoints().getSize ();
    goSize_t i = 0;
    goDouble lastLength = 0.0;
    while (i < (sz - 1) && el && position > pos1)
    {
        goMath::Vector<T> temp = el->elem;
        temp -= el->next->elem;
        lastLength = temp.norm2();
        pos1 += lastLength;
        ++i;
        el = el->next;
    }
    if (position > pos1)
    {
        goLog::warning ("goCurve::sample(): position > pos1 after loop. Out of range?");
        return false;
    }
    if (!el)
    {
        goLog::warning ("goCurve::sample(): el == 0 after loop. Out of range?");
        return false;
    }
    
    if (position > 0.0)
    {
        ret = el->prev->elem + 
            (el->elem - el->prev->elem) * (position - pos1 + lastLength) * (1.0 / lastLength);
    }
    else
    {
        ret = el->elem;
    }
    return true;
}

/** 
 * @brief Resample from start to end given in length parameters.
 *
 * @note Remember that resampling does not preserve the original points
 * except the first and last!
 * 
 * @param start Start (curve goes from 0.0 to getLength())
 * @param end End
 * @param samples Number of samples 
 * @param ret List of points for the result.
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goCurve<T>::resample (goDouble start, goDouble end, 
                           goSize_t samples, goList<goMath::Vector<T> >& ret) const
{
    if (this->getPoints().getSize() < 2)
    {
        return false;
    }
    goDouble pos1 = 0.0;

    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    goSize_t sz = this->getPoints().getSize ();
    goSize_t i = 0;
    goDouble lastLength = 0.0;
    while (i < (sz - 1) && el && start > pos1)
    {
        goMath::Vector<T> temp = el->elem;
        temp -= el->next->elem;
        lastLength = temp.norm2();
        pos1 += lastLength;
        ++i;
        el = el->next;
    }
    if (start > pos1)
    {
        goLog::warning ("goCurve::resample(): start > pos1 after loop. Wrong start value?");
        return false;
    }

    assert (el);

    //= Go one step back
    // --i;
    // el = el->prev;
    // pos1 -= lastLength;
    //= Copy points including start and end points into temporary and
    //= then resample those (this is more readable and less error prone).
    goList<goMath::Vector<T> > tempPoints;
    //= First point:
    if (start > 0.0)
    {
        goMath::Vector<T> p = el->prev->elem + 
            (el->elem - el->prev->elem) * (start - pos1 + lastLength) * (1.0 / lastLength);
        tempPoints.append (p);
    }
    // pos1 += lastLength;
    while (el && pos1 < end && i < sz)
    {
        tempPoints.append (el->elem);
        goMath::Vector<T> temp = el->elem;
        temp -= el->next->elem;
        lastLength = temp.norm2();
        pos1 += lastLength;
        ++i;
        el = el->next;
    }
    if (pos1 < end)
    {
        goLog::warning ("goCurve::resample(): pos1 < end after loop. end too large? -- resampling until end of curve instead.");
    }
    
    //= Append last point.
    {
        goMath::Vector<T> p = el->prev->elem + 
            (el->elem - el->prev->elem) * (end - pos1 + lastLength) * (1.0 / lastLength);
        tempPoints.append (p);
    }

    return resampleLinear (tempPoints.getFrontElement(), tempPoints.getSize(), 
            samples, ret, false);
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

template <class T>
bool goCurve<T>::setPoints (const goMath::Matrix<T>& m)
{
    return goPointCloud<T>::setPoints (m);
}

/**
 */
template <class T>
bool goCurve<T>::setPoints (const goList<goMath::Vector<T> >& l)
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
bool goCurve<T>::setPoints (typename goList<goMath::Vector<T> >::ConstElement* sourceBegin, 
                            goIndex_t                         sourcePointCount, 
                            goIndex_t                         destPointCount,
                            bool                              closed)
{
    goList<goMath::Vector<T> >& points = this->getPoints();
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
bool goCurve<T>::setPoints (typename goList<goMath::Vector<T> >::ConstElement* sourceBegin, goIndex_t sourcePointCount, bool closed)
{
    goList<goMath::Vector<T> >& points = this->getPoints();
    points.erase();
    goIndex_t i;
    typename goList<goMath::Vector<T> >::ConstElement* el = sourceBegin;
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
    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    typename goList<goMath::Vector<T> >::ConstElement* otherEl = 0;
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

/*
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
    goList<goMath::Vector<T> > newPoints;
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
bool goCurve<T>::resample (goIndex_t pointCount, goList<goMath::Vector<T> >& ret) const
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

/* 
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
bool goCurve<T>::resampleNUBS (typename goList<goMath::Vector<T> >::ConstElement* begin, typename goList<goMath::Vector<T> >::ConstElement* end, goIndex_t pointCount, goList<goMath::Vector<T> >& ret)
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
bool goCurve<T>::resample (typename goList<goMath::Vector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goMath::Vector<T> >& ret, bool closedCurve)
{
    return goCurve<T>::resampleLinear (begin,pointCount,resamplePointCount,ret,closedCurve);
}

/* 
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
bool goCurve<pointT>::resampleNUBS (typename goList<goMath::Vector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goMath::Vector<T> >& ret)
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
 * @note This uses linear splines. The original points are interpolated.
 * 
 * @param begin 
 * @param pointCount 
 * @param resamplePointCount 
 * @param ret 
 * 
 * @return True if successful, false otherwise.
 */
template<class T>
bool goCurve<T>::resampleLinear (typename goList<goMath::Vector<T> >::ConstElement* begin, goIndex_t pointCount, goIndex_t resamplePointCount, goList<goMath::Vector<T> >& ret, bool closedCurve)
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
        typename goList<goMath::Vector<T> >::ConstElement* el = begin;
        goIndex_t i;
        for (i = 1; i < pointCount; ++i, el = el->next)
        {
            goMath::Vector<T> p = el->elem - el->next->elem;
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
    typename goList<goMath::Vector<T> >::ConstElement* el = begin;
    for (i = 0; i < resamplePointCount; ++i)
    {
        assert (el && el->next);
        assert (j >= 0 && j < pointCount - 1);
        goMath::Vector<T> p;
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
bool goCurve<T>::readASCII (FILE* f, goList<goMath::Vector<T> >& ret)
{
    if (!f)
    {
        return false;
    }

    bool closed = false;
    
    goString line;
    goFileIOException ex (goFileIOException::UNEXPECTED_DATA);
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
            throw ex;
            return false;
        }
        goList<goString>::Element* el = words.getFrontElement();
        if (el->elem != "curve")
        {
            goString msg = "goCurve::readASCII(): expected 'curve ...', got '";
            msg += line;
            msg += "'";
            goLog::warning(msg);
            throw ex;
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
            throw ex;
            return false;
        }
        goList<goString>::Element* el = words.getFrontElement();
        if (el->elem != "dimension")
        {
            goString msg = "goCurve::readASCII(): expected 'dimension ...', got '";
            msg += line;
            msg += "'";
            goLog::warning(msg);
            throw ex;
            return false;
        }

        dim = el->next->elem.toInt();
    }
    unsigned int pointCount = 0;
    fscanf(f,"%d\n",&pointCount);
    unsigned int i;
    goMath::Vector<T> p(dim);
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
                throw ex;
                return false;
            }
            p[j] = x;
        }
        if (fscanf(f,"%f\n",&x) < 1)
        {
            goLog::warning("goCurve::readASCII(): could not read point from a line.");
            throw ex;
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
bool goCurve<T>::writeASCII (FILE* f, const goList<goMath::Vector<T> >& pointList)
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
    typename goList<goMath::Vector<T> >::ConstElement* el = pointList.getFrontElement();
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
goSize_t goCurve<T>::removeDuplicates (goList<goMath::Vector<T> >& pl)
{
    goSize_t removed = 0;
    goSize_t removed_total = 0;
    do
    {
        goSize_t i = 0;
        removed = 0;
        goSize_t sz = pl.getSize();
        typename goList<goMath::Vector<T> >::Element* el = pl.getFrontElement();
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
 * @brief Removes one of two consecutive when they are not more than epsilon apart.
 * 
 * @param pl 
 * @param epsilon 
 * 
 * @return Number of removed points.
 */
template <class T>
goSize_t goCurve<T>::removeCloseDuplicates (goList<goMath::Vector<T> >& pl, goDouble epsilon)
{
    goSize_t removed = 0;
    goSize_t removed_total = 0;
    do
    {
        goSize_t i = 0;
        removed = 0;
        goSize_t sz = pl.getSize();
        typename goList<goMath::Vector<T> >::Element* el = pl.getFrontElement();
        if (!pl.isClosed())
        {
            --sz;
        }
        while (i < sz && el)
        {
            if ((el->elem - el->next->elem).norm2() <= epsilon)
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
bool goCurve<T>::filter (const goFloat* mask, goSize_t size, goSize_t center, goList<goMath::Vector<T> >& pointList, goSize_t count)
{
    if (pointList.isEmpty())
        return true;

    goMath::Matrix<T> M (pointList.getFront().getSize(), pointList.getSize());
    typename goList<goMath::Vector<T> >::Element* el = pointList.getFrontElement();
    goSize_t sz = pointList.getSize();
    goMath::Vector<T> column;
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
goSize_t goCurve<T>::removeCloseDuplicates (goDouble epsilon)
{
    return goCurve<T>::removeCloseDuplicates (this->getPoints(), epsilon);
}

template <class T>
bool goCurve<T>::writeASCII (FILE* f) const
{
    return goCurve<T>::writeASCII (f, this->getPoints());
}

template <class T>
bool goCurve<T>::writeASCII (const char* filename) const
{
    FILE* f = ::fopen (filename, "w");
    if (!f)
        return false;
    bool ok = goCurve<T>::writeASCII (f, this->getPoints());
    ::fclose (f);
    return ok;
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

template <class T>
bool goCurve<T>::readASCII (const char *fname)
{
    FILE* f = ::fopen (fname, "r");
    if (!f)
        return false;

    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}

template<class T>
bool goCurve<T>::getGradNorm (goArray<goFloat>& diffNorm) const
{
    if (this->getPoints().isEmpty())
        return false;

    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    
    const goMath::Vector<T>* p1 = 0;
    const goMath::Vector<T>* p2 = 0;
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
bool goCurve<T>::getGrad (goList<goMath::Vector<T> >& diff) const
{
    if (this->getPoints().isEmpty())
        return false;
    
    diff.erase();
    goMath::Vector<T> d(this->getDim());
    d.fill (T(0));
    
    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    assert (el);
    const goMath::Vector<T>* p1 = 0;
    const goMath::Vector<T>* p2 = 0;
    goSize_t pointCount = this->getPointCount ();
    goSize_t j = 0;
    while (el && j < pointCount)
    {
        p1 = &el->elem;
        if (el->next)
            p2 = &el->next->elem;
        else
            p2 = p1;
        d = *p2 - *p1;
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

    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    
    const goMath::Vector<T>* p1 = 0;
    const goMath::Vector<T>* p  = 0;
    const goMath::Vector<T>* p2 = 0;
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
bool goCurve<T>::getAngleFunction (goArray<goFloat>& angles, const goMath::Vector<T>& axis) const
{
    if (this->getPoints().isEmpty())
        return false;
    angles.resize (this->getPoints().getSize());
    goMath::Vector<T> a = axis;
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
    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
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
static goDouble getTurn (const goMath::Vector<T>& p1, const goMath::Vector<T>& p2, const goMath::Vector<T>& p3)
{
    goMath::Vector<T> base = p3 - p1;
    goDouble f = base.abs();
    if (f == 0.0)
    {
        assert ("p3 == p1" == 0);
        return 0.0;  //= This should not happen. It would mean p3 == p1.
    }
    base *= 1.0 / f;
    goMath::Vector<T> s1 = p2 - p1;
    goMath::Vector<T> s2 = p3 - p2;
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
bool goCurve<T>::getTurningFunction (goMath::Vector<T>& ret) const
{
    goIndex_t sz = this->getPoints().getSize();

    if (sz <= 0)
        return false;

    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
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

    typename goList<goMath::Vector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    assert (el);
    goIndex_t count = this->getPoints().getSize();
    goDouble length = 0.0;
    goMath::Vector<T> lastPoint = el->elem;
    goMath::Vector<T> d;
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
goDouble goCurve<T>::getLength (const goList<goMath::Vector<T> >& pl) 
{
    if (pl.isEmpty())
        return 0.0;

    typename goList<goMath::Vector<T> >::ConstElement* el = pl.getFrontElement();
    assert (el);
    goIndex_t count = pl.getSize();
    goDouble length = 0.0;
    goMath::Vector<T> lastPoint = el->elem;
    goMath::Vector<T> d;
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
