#include <gopointcloud.h>
#include <gopoint.h>
#include <golist.h>
#include <gostring.h>
#include <gofileio.h>
#include <golog.h>
#include <gomath.h>
#include <assert.h>
#include <goeigenvalue.h>

template <class T>
class goPointCloudPrivate
{
    public:
        goPointCloudPrivate();
        ~goPointCloudPrivate();

        goList<goVector<T> > points;
        goSize_t             dim;
};

template <class T>
goPointCloudPrivate<T>::goPointCloudPrivate ()
    : points (), dim (2)
{
}

template <class pointT>
goPointCloudPrivate<pointT>::~goPointCloudPrivate ()
{
}

// =====================================

template <class T>
goPointCloud<T>::goPointCloud (goSize_t dim)
    : goObjectBase (),
      myPrivate (0)
{
    this->setClassID(GO_POINTCLOUD);
    myPrivate = new goPointCloudPrivate<T>;
    assert (myPrivate);
    myPrivate->dim = dim;
}

template <class T>
goPointCloud<T>::goPointCloud (const goPointCloud<T>& other)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goPointCloudPrivate<T>;
    assert (myPrivate);
    *this = other;
}

template <class T>
goPointCloud<T>::goPointCloud (const goList<goVector<T> >& pl)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goPointCloudPrivate<T>;
    assert (myPrivate);
    this->setPoints (pl);
}

template <class T>
goPointCloud<T>::goPointCloud (const goMatrix<T>& confMatrix)
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new goPointCloudPrivate<T>;
    assert (myPrivate);
    goList<goVector<T> >& points = this->getPoints();
    goSize_t sz = confMatrix.getRows();
    const goVector<T> ref;
    for (goSize_t i = 0; i < sz; ++i)
    {
        confMatrix.refRow (i, ref);
        points.append (ref);
    }
}

template <class T>
goPointCloud<T>& goPointCloud<T>::operator= (const goPointCloud<T>& other)
{
    *myPrivate = *other.myPrivate;
    return *this;
}

template <class T>
goPointCloud<T>::~goPointCloud ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

template <class T>
bool goPointCloud<T>::operator!= (const goPointCloud<T>& other) const
{
    return myPrivate->points != other.myPrivate->points;
}

template <class T>
bool goPointCloud<T>::operator== (const goPointCloud<T>& other) const
{
    return !(*this != other);
}

/** 
 * @brief Get number of points.
 * 
 * @return Number of points in the point cloud.
 */
template <class T>
goIndex_t goPointCloud<T>::getPointCount () const
{
    assert (myPrivate);
    return myPrivate->points.getSize();
}

/**
* @brief Get list of points.
*
* @return Reference to the list of pointT objects constituting the point cloud.
**/
template <class T>
goList<goVector<T> >& goPointCloud<T>::getPoints ()
{
    return myPrivate->points;
}

template <class T>
void goPointCloud<T>::affineTransform (const goMatrix<T>& m)
{
    typename goList<goVector<T> >::Element* el = this->getPoints().getFrontElement();
    goSize_t sz = this->getPoints().getSize();
    goSize_t i;
    if (el)
    {
        assert (el->elem.getSize() == m.getRows() - 1 && m.getRows() == m.getColumns());
    }
    for (i = 0; i < sz; ++i)
    {
        goVector<T> temp (this->getDim() + 1);
        el->elem.copy (temp, 0, 0);
        // temp = el->elem;
        temp[temp.getSize()-1] = 1.0;
        //temp *= m;
        temp = m * temp;
        temp *= 1.0 / temp[temp.getSize()-1];
        temp.copy (el->elem, 0, 0, el->elem.getSize()-1);
        //el->elem[3] = 1.0;
        //el->elem *= m;
        //el->elem *= 1.0 / el->elem[3];
        //el->elem[3] = 0.0;
        el = el->next;
    }
}

template <class T>
bool goPointCloud<T>::readASCII (const char* filename, goSize_t dimension, goList<goVector<T> >& pointList)
{
    FILE* f = ::fopen (filename, "r");
    if (!f)
    {
        goString msg ("goPointCloud::readASCII(): Could not open file ");
        msg += filename;
        goLog::warning (msg);
        return false;
    }

    goString line;
    while (goFileIO::readASCIILine(f, line))
    {
        goList<goString> words;
        line.getWords (words);
        if (words.getSize() > 0)
        {
            //= Comments
            if (words.getFront()[0] == '#')
            {
                continue;
            }
            //= Expecting numbers
            goList<goString>::Element* el = words.getFrontElement();
            goVector<T> v (dimension);
            goSize_t i = 0;
            while (el && i < dimension)
            {
                v[i] = el->elem.toFloat();
                el = el->next;
                ++i;
            }
            pointList.append (v);
        }
    }
    ::fclose (f);
    return true;
}

/**
* @brief Get list of points.
*
* @return Reference to the list of goPointf objects constituting the point cloud.
**/
template <class T>
const goList<goVector<T> >& goPointCloud<T>::getPoints () const
{
    return myPrivate->points;
}

/** 
 * @brief Set points from the rows of a configuration matrix.
 * 
 * @param m Matrix, each row containing one point.
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goPointCloud<T>::setPoints (const goMatrix<T>& m)
{
    myPrivate->points.erase ();
    goSize_t sz = m.getRows ();
    const goVector<T> ref;
    for (goSize_t i = 0; i < sz; ++i)
    {
        m.refRow (i, ref);
        myPrivate->points.append (ref);
    }
    if (!myPrivate->points.isEmpty())
    {
        myPrivate->dim = m.getColumns();
    }
    return true;
}

/**
* @brief Set the point list.
*
* @param points  List of points. This list will be deep-copied into the internal list.
**/
template <class T>
bool goPointCloud<T>::setPoints (const goList<goVector<T> >& points)
{
    myPrivate->points = points;
    if (!points.isEmpty())
    {
        assert (points.getFrontElement());
        myPrivate->dim = points.getFrontElement()->elem.getSize();
    }
    return true;
}

/** 
 * @brief Adds point to this cloud.
 * 
 * @param p Point to be added.
 */
template <class T>
void goPointCloud<T>::addPoint (const goVector<T>& p)
{
    assert (p.getSize() == this->getDim());
    myPrivate->points.append(p);
}

template <class T>
void goPointCloud<T>::setChanged ()
{
    this->sendObjectMessage (GO_OBJECTMESSAGE_CHANGED, 0);
}

template <class T>
goSize_t goPointCloud<T>::getDim () const
{
    return myPrivate->dim;
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
template <class T>
bool goPointCloud<T>::getCenterOfMass (goVector<T>& comRet) const
{
    if (myPrivate->points.isEmpty())
        return false;

    goIndex_t pointCount = static_cast<goIndex_t>(myPrivate->points.getSize());
    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
//    goDouble x = 0.0;
//    goDouble y = 0.0;
//    goDouble z = 0.0;
//    goDouble w = 0.0;
    goVector<T> temp (this->getDim());
    temp.fill (0.0);
    typename goList<goVector<T> >::ConstElement* el = myPrivate->points.getFrontElement();
    assert(el);
    goIndex_t i = 0;
    while (el && i < pointCount)
    {
//        p = &el->elem;           // myPrivate->points.getCurrentPtr();
//        assert (p);
        temp += el->elem * factor;        
//        x += p->x * factor;
//        y += p->y * factor;
//        z += p->z * factor;
//        w += p->w * factor;
        el = el->next;
        ++i;
    }
    comRet = temp;
//    comRet.x = x;
//    comRet.y = y;
//    comRet.z = z;
//    comRet.w = w;
    return true;
}

template <class T>
bool goPointCloud<T>::getCenterOfMass (const goFixedArray<goVector<T> >& points, goVector<T>& comRet)
{
    if (points.getSize() < 1)
        return false;

    goIndex_t pointCount = static_cast<goIndex_t>(points.getSize());
    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
    goVector<T> temp (points[0].getSize());
    temp.fill (T(0));
    goIndex_t i = 0;
    while (i < pointCount)
    {
        temp += points[i] * factor;        
        ++i;
    }
    comRet = temp;
//    comRet.x = x;
//    comRet.y = y;
//    comRet.z = z;
//    comRet.w = w;
    return true;
}

/**
* @brief Translate all points.
*
* @param d  Translation.
*
* @return True if successful, false otherwise.
**/
template <class T>
bool goPointCloud<T>::translate (const goVector<T>& d)
{
    if (myPrivate->points.isEmpty())
        return true;
    
    goIndex_t pointCount = static_cast<goIndex_t>(myPrivate->points.getSize());
    typename goList<goVector<T> >::Element* el = myPrivate->points.getFrontElement();
    goIndex_t i = 0;
    while (el && i < pointCount)
    {
        el->elem += d;
        el = el->next;
        ++i;
    }
    return true;
}

/** 
 * @brief Get matrix containing the point coordinates.
 * 
 * Fill a matrix \f$M \in R^{k\times n}\f$, where
 * with the k points of dimension n from this point cloud.
 *
 * @param cmRet Configuration matrix. Resized if necessary.
 */
template <class T>
void goPointCloud<T>::getConfigurationMatrix (goMatrix<T>& cmRet) const
{
    goSize_t k = this->getPointCount();
    goSize_t n = this->getDim();
    if (cmRet.getRows() != k || cmRet.getColumns() != n)
    {
        cmRet.resize (k, n);
    }
    goSize_t i = 0, j = 0;
    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    while (el && i < k)
    {
        for (j = 0; j < n; ++j)
        {
            cmRet(i,j) = el->elem[j];
        }
        el = el->next;
        ++i;
    }
    assert (i == k);
}

/** 
 * @brief Get configuration vector.
 *
 * Get a vector \f$v = (p_0,\dots,p_{k-1})^\top \in R^{k \cdot n}\f$ containing
 * the coordinates of all k points p of dimension n in this point cloud concatenated.
 * 
 * @param cvRet Configuration vector. Resized if necessary.
 */
template <class T>
void goPointCloud<T>::getConfigurationVector (goVector<T>& cvRet) const
{
    goSize_t k = this->getPointCount();
    goSize_t n = this->getDim();
    if (cvRet.getSize() != n * k)
    {
        cvRet.resize (k * n);
    }
    goSize_t i = 0;
    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    while (el && i < k * n)
    {
        goSize_t j;
        for (j = 0; j < n; ++j, ++i)
        {
            cvRet(i) = el->elem[j];
        }
        el = el->next;
    }
    assert (i == k * n);
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
    
    typename goList<goVector<T> >::Element* el = myPrivate->points.getFrontElement();
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
template <class T>
bool goPointCloud<T>::getPrincipalAxes2D (goVectorf& a1, goVectorf& a2, const goArray<goFloat>* weights) const
{
    //= Moment-of-inertia tensor
    goDouble I_00 = 0.0;
    goDouble I_01 = 0.0;
    goDouble I_11 = 0.0;

    a1.setSize (2);
    a2.setSize (2);

    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
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
            I_00 += (*weights)[i] * el->elem[0] * el->elem[0];
            I_11 += (*weights)[i] * el->elem[1] * el->elem[1];
            I_01 += (*weights)[i] * el->elem[0] * el->elem[1];
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
            I_00 += el->elem[0] * el->elem[0];
            I_11 += el->elem[1] * el->elem[1];
            I_01 += el->elem[0] * el->elem[1];
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
    a1.fill (0.0);
    a1[0] = 1.0f;
    a1[1] = (l1-I_00)/I_01;
    a1 *= 1.0f / a1.abs();
    a2.fill (0.0);
    a2[0] = 1.0f;
    a2[1] = (l2-I_00)/I_01;
    a2 *= 1.0f / a2.abs();

    //= Make sure they are orthogonal.
    //= Removed when removing goPoint dependencies
//    go4Vectorf temp;
//    temp = a2;
//    temp.cross(a1);
//    a2 = a1;
//    a2.cross (temp);
//    a2 *= 1.0f / a1.abs();
    return true;
}

template <class T>
bool goPointCloud<T>::getPrincipalAxes (goMatrix<T>& axes) const
{
    typename goList<goVector<T> >::ConstElement* el = this->getPoints().getFrontElement();
    if (!el)
        return false;

    axes.resize (el->elem.getSize(), el->elem.getSize());

    goDouble totalWeight = (goDouble)this->getPoints().getSize();
    {
        goIndex_t i = 0;
        goIndex_t size = this->getPointCount ();
        goVector<T> mean;
        this->getCenterOfMass (mean);
        goMatrix<T> cov (axes.getRows() < axes.getColumns());
        cov.fill (T(0));
        while (el && i < size)
        {
            goVector<T> temp = el->elem - mean;
            goVectorOuter<T> (1.0f, temp, temp, cov);
            el = el->next;
            ++i;
        }
        cov *= 1.0 / totalWeight;

        goMath::goEigenvalue<T> ev (cov);
        ev.getV (axes);
        axes.transpose ();
        //= Normalise
        goSize_t N = axes.getRows ();
        for (goSize_t j = 0; j < N; ++j)
        {
            goVector<T> r;
            axes.refRow (j, r);
            T norm = r.norm2 ();
            if (norm > 1e-4)
            {
                r *= 1.0f / norm;
            }
        }
    }
    return true;
}

template <class T>
bool goPointCloud<T>::getPrincipalAxes (const goFixedArray<goVector<T> >& points, goMatrix<T>& axes)
{
    if (points.getSize() < 3)
    {
        return false;
    }

    axes.resize (points[0].getSize(), points[0].getSize());

    goDouble totalWeight = (goDouble)points.getSize();
    {
        goIndex_t i = 0;
        goIndex_t size = points.getSize();
        goVector<T> mean;
        goPointCloud<T>::getCenterOfMass (points, mean);
        goMatrix<T> cov (axes.getRows() < axes.getColumns());
        cov.fill (T(0));
        while (i < size)
        {
            goVector<T> temp = points[i] - mean;
            goVectorOuter<T> (1.0f, temp, temp, cov);
            ++i;
        }
        cov *= 1.0 / totalWeight;

        goMath::goEigenvalue<T> ev (cov);
        ev.getV (axes);
        axes.transpose ();
        //= Normalise
        goSize_t N = axes.getRows ();
        for (goSize_t j = 0; j < N; ++j)
        {
            goVector<T> r;
            axes.refRow (j, r);
            T norm = r.norm2 ();
            if (norm > 1e-4)
            {
                r *= 1.0f / norm;
            }
        }
        printf ("axes: \n");
        axes.print();
    }
    return true;
}

template <class T>
bool goPointCloud<T>::unitScale (goFloat f)
{
    if (myPrivate->points.isEmpty())
        return false;
    assert (f >= 0.0f);
    if (f < 0.0)
    {
        return false;
    }
    typename goList<goVector<T> >::Element* el = myPrivate->points.getFrontElement();
    goVector<T> max = el->elem;
    goVector<T> min = el->elem;
    const goSize_t N = max.getSize ();
    goSize_t i;
    goSize_t j = 0;
    const goSize_t pointCount = this->getPointCount();
    while (el && j < pointCount)
    {
        for (i = 0; i < N; ++i)
        {
            if (el->elem[i] > max[i])
            {
                max[i] = el->elem[i];
            }
            if (el->elem[i] < min[i])
            {
                min[i] = el->elem[i];
            }
        }
        el = el->next;
        ++j;
    }
    goVector<T> trans (min);
    goVector<T> factorv (this->getDim());
    factorv.fill (f);
    for (i = 0; i < N; ++i)
    {
        if (max[i] != min[i])
        {
            factorv[i] = f / (max[i] - min[i]);
        }
    }
//    if (max.x != min.x)
//        factorv.x = f / (max.x - min.x);
//    if (max.y != min.y)
//        factorv.y = f / (max.y - min.y);
    goFloat factor = factorv[0];
    for (i = 0; i < N; ++i)
    {
        if (factorv[i] < factor)
        {
            factor = factorv[i];
        }
    }
//    goMath::min (factorv.x, factorv.y);
    el = myPrivate->points.getFrontElement();
    j = 0;
    while (el && j < pointCount)
    {
        el->elem -= trans;
        el->elem *= factor;
//        el->elem.x -= trans.x;
//        el->elem.x *= factor;
//        el->elem.y -= trans.y;
//        el->elem.y *= factor;
        el = el->next;
        ++j;
    }
    return true;
}

/**
* @brief DO NOT USE Write the point cloud to a file.
*
* Writes all 4 coordinates and the point value to the file.
* 
* @param f  Valid, open file.
*
* @return True if successful, false otherwise.
**/
template <class T>
bool goPointCloud<T>::writeObjectFile (FILE* f) const
{
    if (!f)
        return false;
    if (!goFileIO::writeASCII (f, goString("goPointCloud\n")))
        return false;
//    const char cnull = 0;
//    fwrite (&cnull, sizeof(char), 1, f);
//    fwrite (&s, sizeof(goIndex_t), 1, f);
    typename goList<goVector<T> >::ConstElement* el = myPrivate->points.getFrontElement();
    goSize_t j = 0;
    goSize_t pointCount = this->getPointCount ();
    const goSize_t N = (el ? el->elem.getSize() : 0);
    goString numString = "";
    numString += (int)N;
    numString += "\n";
    if (!goFileIO::writeASCII (f, numString))
    {
        return false;
    }
    numString = "";
    numString += (int)pointCount;
    numString += "\n";
    if (!goFileIO::writeASCII (f, numString))
    {
        return false;
    }
    goSize_t i;
    while (el && j < pointCount)
    {
        for (i = 0; i < N; ++i)
        {
            fwrite (&el->elem[i], sizeof(goFloat), 1, f);
        }
//        fwrite (&el->elem.x, sizeof(goFloat), 1, f);
//        fwrite (&el->elem.y, sizeof(goFloat), 1, f);
//        fwrite (&el->elem.z, sizeof(goFloat), 1, f);
//        fwrite (&el->elem.w, sizeof(goFloat), 1, f);
//        fwrite (&el->elem.value, sizeof(goDouble), 1, f);
        el = el->next;
        ++j;
    }
    return goObjectBase::writeObjectFile (f);
}

/**
* @brief DO NOT USE Read the point cloud from a file.
*
* Reads all 4 coordinates and the point value from the file.
* 
* @param f  Valid, open file.
*
* @return True if successful, false otherwise.
**/
template <class T>
bool goPointCloud<T>::readObjectFile (FILE* f)
{
    if (!f)
        return false;
    goSize_t s = 0;
    goString name;
    if (!goFileIO::readASCIILine (f, name))
        return false;
    if (name != "goPointCloud")
        return false;
 //   fread (&s, sizeof(goIndex_t), 1, f);
    goString line = "";
    if (!goFileIO::readASCIILine (f, line))
    {
        return false;
    }
    goSize_t N = 0;
    N = line.toInt ();
    if (!goFileIO::readASCIILine (f, line))
    {
        return false;
    }
    s = line.toInt ();
    myPrivate->points.erase();
    goVector<T> p (N);
    goSize_t count = 0;
    goSize_t i;
    goSize_t j;
    for (i = 0; i < s; ++i)
    {
        count = 0;
        for (j = 0; j < N; ++j)
        {
            count += fread (&p[j], sizeof(goFloat), 1, f);
        }
//        count += fread (&p.x, sizeof(goFloat), 1, f);
//        count += fread (&p.y, sizeof(goFloat), 1, f);
//        count += fread (&p.z, sizeof(goFloat), 1, f);
//        count += fread (&p.w, sizeof(goFloat), 1, f);
//        count += fread (&p.value, sizeof(goDouble), 1, f);
        if (count != N)
            return false;
        myPrivate->points.append (p);
    }

    return goObjectBase::readObjectFile (f);
}

template <class T>
bool goPointCloud<T>::callObjectMethod (int methodID, goObjectMethodParameters* param)
{
    return goObjectBase::callObjectMethod (methodID, param);
}

template <class T>
void goPointCloud<T>::receiveObjectMessage (const goObjectMessage& msg)
{
    goObjectBase::receiveObjectMessage (msg);
}

template class goPointCloud <goFloat>;
template class goPointCloud <goDouble>;
