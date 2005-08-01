#include <gopointcloud.h>
#include <gopoint.h>
#include <golist.h>
#include <gostring.h>
#include <gofileio.h>
#include <golog.h>
#include <gomath.h>
#include <assert.h>

class goPointCloudPrivate
{
    public:
        goPointCloudPrivate();
        ~goPointCloudPrivate();

        goList<goPointf> points;
};

goPointCloudPrivate::goPointCloudPrivate ()
    : points ()
{
}

goPointCloudPrivate::~goPointCloudPrivate ()
{
}

// =====================================

goPointCloud::goPointCloud ()
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassName ("goPointCloud");
    myPrivate = new goPointCloudPrivate;
    assert (myPrivate);
}

goPointCloud::goPointCloud (const goPointCloud& other)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goPointCloudPrivate;
    assert (myPrivate);
    *this = other;
}

goPointCloud& goPointCloud::operator= (const goPointCloud& other)
{
    *myPrivate = *other.myPrivate;
    return *this;
}

goPointCloud::~goPointCloud ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goPointCloud::operator!= (const goPointCloud& other)
{
    return myPrivate->points != other.myPrivate->points;
}

goList<goPointf>& goPointCloud::getPoints ()
{
    return myPrivate->points;
}

const goList<goPointf>& goPointCloud::getPoints () const
{
    return myPrivate->points;
}

void goPointCloud::setPoints (goList<goPointf>& points)
{
    myPrivate->points = points;
}

bool goPointCloud::getCenterOfMass (goPointf& comRet) const
{
    if (myPrivate->points.isEmpty())
        return false;

    const goPointf* p = 0;
    goDouble factor = 1.0 / static_cast<goDouble>(myPrivate->points.getSize());
    goDouble x = 0.0;
    goDouble y = 0.0;
    goDouble z = 0.0;
    goDouble w = 0.0;
    goList<goPointf>::ConstElement* el = myPrivate->points.getFrontElement();
    assert(el);
    while (true)
    {
        p = &el->elem;           // myPrivate->points.getCurrentPtr();
        assert (p);
        x += p->x * factor;
        y += p->y * factor;
        z += p->z * factor;
        w += p->w * factor;
        if (!el->next)      // myPrivate->points.isTail())
            break;
        el = el->next;
    }
    comRet.x = x;
    comRet.y = y;
    comRet.z = z;
    comRet.w = w;
    return true;
}

bool goPointCloud::translate (const goPointf& d)
{
    if (myPrivate->points.isEmpty())
        return true;
    
    myPrivate->points.resetToFront();
    goPointf* p = 0;
    while (true)
    {
        p = myPrivate->points.getCurrentPtr();
        assert (p);
        *p += d;
        if (myPrivate->points.isTail())
            break;
        myPrivate->points.next();
    }
    return true;
}

bool goPointCloud::transform (const go44Matrixf& m)
{
    if (myPrivate->points.isEmpty())
        return true;
    
    goList<goPointf>::Element* el = myPrivate->points.getFrontElement();
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
bool goPointCloud::getPrincipalAxes2D (go4Vectorf& a1, go4Vectorf& a2, const goArray<goFloat>* weights) const
{
    //= Moment-of-inertia tensor
    goDouble I_00 = 0.0;
    goDouble I_01 = 0.0;
    goDouble I_11 = 0.0;

    goList<goPointf>::ConstElement* el = this->getPoints().getFrontElement();
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

bool goPointCloud::unitScale (goFloat f)
{
    if (myPrivate->points.isEmpty())
        return false;
    goList<goPointf>::Element* el = myPrivate->points.getFrontElement();
    goPointf max = el->elem;
    goPointf min = el->elem;
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

bool goPointCloud::writeObjectFile (FILE* f) const
{
    if (!f)
        return false;
    goIndex_t s = myPrivate->points.getSize();
    if (!goFileIO::writeASCII (f, goString("goPointCloud")))
        return false;
    const char cnull = 0;
    fwrite (&cnull, sizeof(char), 1, f);
    fwrite (&s, sizeof(goIndex_t), 1, f);
    goList<goPointf>::ConstElement* el = myPrivate->points.getFrontElement();
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

bool goPointCloud::readObjectFile (FILE* f)
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
    goPointf p;
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

bool goPointCloud::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    return goObjectBase::callObjectMethod (methodID, param);
}

void goPointCloud::receiveObjectMessage (const goObjectMessage& msg)
{
    goObjectBase::receiveObjectMessage (msg);
}
