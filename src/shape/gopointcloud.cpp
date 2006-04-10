#include <gopointcloud.h>
#include <gopoint.h>
#include <golist.h>
#include <gostring.h>
#include <gofileio.h>
#include <golog.h>
#include <gomath.h>
#include <assert.h>

template <class pointT>
class goPointCloudPrivate
{
    public:
        goPointCloudPrivate();
        ~goPointCloudPrivate();

        goList<pointT> points;
};

template <class pointT>
goPointCloudPrivate<pointT>::goPointCloudPrivate ()
    : points ()
{
}

template <class pointT>
goPointCloudPrivate<pointT>::~goPointCloudPrivate ()
{
}

// =====================================

template <class pointT>
goPointCloud<pointT>::goPointCloud ()
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassName ("goPointCloud");
    myPrivate = new goPointCloudPrivate<pointT>;
    assert (myPrivate);
}

template <class pointT>
goPointCloud<pointT>::goPointCloud (const goPointCloud<pointT>& other)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goPointCloudPrivate<pointT>;
    assert (myPrivate);
    *this = other;
}

template <class pointT>
goPointCloud<pointT>::goPointCloud (const goList<pointT>& pl)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goPointCloudPrivate<pointT>;
    assert (myPrivate);
    this->setPoints (pl);
}
template <class pointT>
goPointCloud<pointT>& goPointCloud<pointT>::operator= (const goPointCloud<pointT>& other)
{
    *myPrivate = *other.myPrivate;
    return *this;
}

template <class pointT>
goPointCloud<pointT>::~goPointCloud ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

template <class pointT>
bool goPointCloud<pointT>::operator!= (const goPointCloud<pointT>& other)
{
    return myPrivate->points != other.myPrivate->points;
}

/** 
 * @brief Get number of points.
 * 
 * @return Number of points in the point cloud.
 */
template <class pointT>
goIndex_t goPointCloud<pointT>::getPointCount () const
{
    assert (myPrivate);
    return myPrivate->points.getSize();
}

/**
* @brief Get list of points.
*
* @return Reference to the list of pointT objects constituting the point cloud.
**/
template <class pointT>
goList<pointT>& goPointCloud<pointT>::getPoints ()
{
    return myPrivate->points;
}

/**
* @brief Get list of points.
*
* @return Reference to the list of goPointf objects constituting the point cloud.
**/
template <class pointT>
const goList<pointT>& goPointCloud<pointT>::getPoints () const
{
    return myPrivate->points;
}

/**
* @brief Set the point list.
*
* @param points  List of points. This list will be deep-copied into the internal list.
**/
template <class pointT>
bool goPointCloud<pointT>::setPoints (const goList<pointT>& points)
{
    myPrivate->points = points;
    return true;
}

/** 
 * @brief Adds point to this cloud.
 * 
 * @param p Point to be added.
 */
template <class pointT>
void goPointCloud<pointT>::addPoint (const pointT& p)
{
    myPrivate->points.append(p);
}

template <class pointT>
void goPointCloud<pointT>::setChanged ()
{
    this->sendObjectMessage (GO_OBJECTMESSAGE_CHANGED, 0);
}

/**
* @brief Calculates the center of mass of the point cloud.
*
* No weights are used, all points are weighted with the factor 1.
* 
* @param comRet  Center of mass.
*
* @return True if successful, false otherwise.
**/
template <class pointT>
bool goPointCloud<pointT>::getCenterOfMass (pointT& comRet) const
{
    if (myPrivate->points.isEmpty())
        return false;

    const pointT* p = 0;
    goIndex_t pointCount = static_cast<goIndex_t>(myPrivate->points.getSize());
    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
    goDouble x = 0.0;
    goDouble y = 0.0;
    goDouble z = 0.0;
    goDouble w = 0.0;
    typename goList<pointT>::ConstElement* el = myPrivate->points.getFrontElement();
    assert(el);
    goIndex_t i = 0;
    while (el && i < pointCount)
    {
        p = &el->elem;           // myPrivate->points.getCurrentPtr();
        assert (p);
        x += p->x * factor;
        y += p->y * factor;
        z += p->z * factor;
        w += p->w * factor;
        el = el->next;
        ++i;
    }
    comRet.x = x;
    comRet.y = y;
    comRet.z = z;
    comRet.w = w;
    return true;
}

/**
* @brief Translate all points.
*
* @param d  Translation.
*
* @return True if successful, false otherwise.
**/
template <class pointT>
bool goPointCloud<pointT>::translate (const pointT& d)
{
    if (myPrivate->points.isEmpty())
        return true;
    
    goIndex_t pointCount = static_cast<goIndex_t>(myPrivate->points.getSize());
    typename goList<pointT>::Element* el = myPrivate->points.getFrontElement();
    goIndex_t i = 0;
    while (el && i < pointCount)
    {
        el->elem += d;
        el = el->next;
        ++i;
    }
    return true;
}

// Now in the header file as template.
#if 0
/*
* @brief Transform all points with a 4x4-Matrix m.
*
* @param m  Transform matrix.
*
* @return True if successful, false otherwise.
**/
template <class pointT>
bool goPointCloud<pointT>::transform (const go44Matrixf& m)
{
    if (myPrivate->points.isEmpty())
        return true;
    
    typename goList<pointT>::Element* el = myPrivate->points.getFrontElement();
    assert (el);
    while (true)
    {
        el->elem *= m;
        if (!el->next)
            break;
        el = el->next;
    }
    return true;
}
#endif

/**
 * @brief  Calculate 2 principal axes.
 * 
 * Works only with x and y coordinates of the points.
 *
 * Points must be moved to their center of mass prior to this method.
 * 
 * @param a1  Contains the major principal axis (the one with the higher eigenvalue).
 * @param a2  Contains the minor principal axis.
 *
 * @return True if successful, false otherwise.
 **/
template <class pointT>
bool goPointCloud<pointT>::getPrincipalAxes2D (go4Vectorf& a1, go4Vectorf& a2, const goArray<goFloat>* weights) const
{
    //= Moment-of-inertia tensor
    goDouble I_00 = 0.0;
    goDouble I_01 = 0.0;
    goDouble I_11 = 0.0;

    typename goList<pointT>::ConstElement* el = this->getPoints().getFrontElement();
    if (!el)
        return false;

    goDouble totalWeight = 0.0;
    if (weights)
    {
        if (weights->getSize() != this->getPoints().getSize())
        {
            goString msg;
            msg = "getPrincipalAxes2D(): insufficient number of weights. Number of weights is ";
            msg += (int)weights->getSize();
            msg += " and size of points is ";
            msg += (int)this->getPoints().getSize();
            goLog::warning (msg,this);
            return false;
        }
        goIndex_t i = 0;
        goIndex_t size = weights->getSize();
        while (true && i < size)
        {
            I_00 += (*weights)[i] * el->elem.x*el->elem.x;
            I_11 += (*weights)[i] * el->elem.y*el->elem.y;
            I_01 += (*weights)[i] * el->elem.x*el->elem.y;
            totalWeight += (*weights)[i];
            if (!el->next)
                break;
            el = el->next;
            ++i;
        }
    }
    else
    {
        while (true)
        {
            I_00 += el->elem.x*el->elem.x;
            I_11 += el->elem.y*el->elem.y;
            I_01 += el->elem.x*el->elem.y;
            if (!el->next)
                break;
            el = el->next;
        }
        totalWeight = (goDouble)this->getPoints().getSize();
    }
    if (totalWeight == 0.0)
    {
        return false;
    }
    I_00 *= 1.0f / totalWeight;
    I_11 *= 1.0f / totalWeight;
    I_01 *= 1.0f / totalWeight;
    
    goDouble p = -I_00-I_11;
    goDouble q = I_00*I_11-I_01*I_01;
    goDouble S = p*p*0.25-q;
    //= Degenerate points (one point or a line)
    if (S < 0.0)
        return false;
    //= Two eigenvalues
    S = sqrt(S);
    goDouble l1 = -p*0.5+S;
    goDouble l2 = -p*0.5-S;
    if (I_01 == 0.0)
        return false;
    if (fabs(l1) < fabs(l2))
    {
        goDouble temp = l1;
        l1 = l2;
        l2 = temp;
    }
    a1.x = 1.0f;
    a1.y = (l1-I_00)/I_01;
    a1.z = 0.0f;
    a1.w = 0.0f;
    a1 *= 1.0f / a1.abs();
    a2.x = 1.0f;
    a2.y = (l2-I_00)/I_01;
    a2.z = 0.0f;
    a2.w = 0.0f;
    a2 *= 1.0f / a2.abs();

    //= Make sure they are orthogonal.
    go4Vectorf temp;
    temp = a2;
    temp.cross(a1);
    a2 = a1;
    a2.cross (temp);
    a2 *= 1.0f / a1.abs();
    return true;
}

template <class pointT>
bool goPointCloud<pointT>::unitScale (goFloat f)
{
    if (myPrivate->points.isEmpty())
        return false;
    typename goList<pointT>::Element* el = myPrivate->points.getFrontElement();
    pointT max = el->elem;
    pointT min = el->elem;
    while (true)
    {
        if (el->elem.x > max.x)
        {
            max.x = el->elem.x;
        }
        if (el->elem.y > max.y)
        {
            max.y = el->elem.y;
        }
        if (el->elem.x < min.x)
        {
            min.x = el->elem.x;
        }
        if (el->elem.y < min.y)
        {
            min.y = el->elem.y;
        }
        if (!el->next)
            break;
        el = el->next;
    }
    el = myPrivate->points.getFrontElement();
    goPointf trans (min.x, min.y);
    goPointf factorv (f, f);
    if (max.x != min.x)
        factorv.x = f / (max.x - min.x);
    if (max.y != min.y)
        factorv.y = f / (max.y - min.y);
    goFloat factor = goMath::min (factorv.x, factorv.y);
    while (true)
    {
        el->elem.x -= trans.x;
        el->elem.x *= factor;
        el->elem.y -= trans.y;
        el->elem.y *= factor;
        if (!el->next)
            break;
        el = el->next;
    }
    return true;
}

/**
* @brief Write the point cloud to a file.
*
* Writes all 4 coordinates and the point value to the file.
* 
* @param f  Valid, open file.
*
* @return True if successful, false otherwise.
**/
template <class pointT>
bool goPointCloud<pointT>::writeObjectFile (FILE* f) const
{
    if (!f)
        return false;
    goIndex_t s = myPrivate->points.getSize();
    if (!goFileIO::writeASCII (f, goString("goPointCloud")))
        return false;
    const char cnull = 0;
    fwrite (&cnull, sizeof(char), 1, f);
    fwrite (&s, sizeof(goIndex_t), 1, f);
    typename goList<pointT>::ConstElement* el = myPrivate->points.getFrontElement();
    while (el)
    {
        fwrite (&el->elem.x, sizeof(goFloat), 1, f);
        fwrite (&el->elem.y, sizeof(goFloat), 1, f);
        fwrite (&el->elem.z, sizeof(goFloat), 1, f);
        fwrite (&el->elem.w, sizeof(goFloat), 1, f);
        fwrite (&el->elem.value, sizeof(goDouble), 1, f);
        el = el->next;
    }
    return goObjectBase::writeObjectFile (f);
}

/**
* @brief Read the point cloud from a file.
*
* Reads all 4 coordinates and the point value from the file.
* 
* @param f  Valid, open file.
*
* @return True if successful, false otherwise.
**/
template <class pointT>
bool goPointCloud<pointT>::readObjectFile (FILE* f)
{
    if (!f)
        return false;
    goIndex_t s = 0;
    goString name;
    if (!goFileIO::readASCII (f, name))
        return false;
    if (name != "goPointCloud")
        return false;
    fread (&s, sizeof(goIndex_t), 1, f);
    if (s < 0)
    {
        return false;
    }
    myPrivate->points.erase();
    pointT p;
    goSize_t count = 0;
    for (goIndex_t i = 0; i < s; ++i)
    {
        count = 0;
        count += fread (&p.x, sizeof(goFloat), 1, f);
        count += fread (&p.y, sizeof(goFloat), 1, f);
        count += fread (&p.z, sizeof(goFloat), 1, f);
        count += fread (&p.w, sizeof(goFloat), 1, f);
        count += fread (&p.value, sizeof(goDouble), 1, f);
        if (count != 5)
            return false;
        myPrivate->points.append (p);
    }

    return goObjectBase::readObjectFile (f);
}

template <class pointT>
bool goPointCloud<pointT>::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    return goObjectBase::callObjectMethod (methodID, param);
}

template <class pointT>
void goPointCloud<pointT>::receiveObjectMessage (const goObjectMessage& msg)
{
    goObjectBase::receiveObjectMessage (msg);
}

template class goPointCloud <goPointf>;
template class goPointCloud <goPointd>;
