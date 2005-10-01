#include <gocurve.h>
#include <gopoint.h>
#include <golist.h>
#include <gonurbs.h>
#include <goconfig.h>

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

/**
 * @brief Uniformly resamples the curve with the given number of points.
 *
 * @param pointCount  Number of points. Must be > 1.
 * @param ret         Resampled curve.
 *
 * @return True if successful, false otherwise.
 **/
template<class pointT>
bool goCurve<pointT>::resample (goIndex_t pointCount, goCurve& ret)
{
    if (pointCount <= 1)
        return false;

    goNURBS nurbs (this);
    goList<pointT> newPoints;
    goFloat t = 0.0f;
    goFloat step = nurbs.getCurveLength() / (float)(pointCount - 1);
    goIndex_t i;
    pointT p;
    for (i = 0; i < pointCount; ++i, t += step)
    {
        p = nurbs (t);
        newPoints.append (p);
    }
    ret.setPoints (newPoints);
    return true;
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
