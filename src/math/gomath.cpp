#include <gomath.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOPOINT_H
# include <gopoint.h>
#endif

// Constants

/**
 * @brief Constant (small) epsilon.
 * 
 * Currently set to 1e-6.
 **/
const goFloat goMath::epsilon (1e-6);

template <class T>
bool goMath::centerOfMass (const goList<goVector<T> >& points, goVector<T>& comRet)
{
    if (points.isEmpty())
        return false;

    const goVector<T>* p = 0;
    goIndex_t pointCount = static_cast<goIndex_t>(points.getSize());
    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
    typename goList<goVector<T> >::ConstElement* el = points.getFrontElement();
    assert(el);
    goSize_t sz = 0;
    if (el) 
        sz = el->elem.getSize();
    comRet.setSize(sz);
    comRet.fill (T(0));
    goIndex_t i = 0;
    while (el && i < pointCount)
    {
        p = &el->elem;           // myPrivate->points.getCurrentPtr();
        assert (p);
        assert (p->getSize() == sz);
        comRet += *p * factor;
        el = el->next;
        ++i;
    }
    return true;
}

template <class T>
bool goMath::centerOfMass (const goMatrix<T>& confMatrix, goVector<T>& comRet)
{
    goIndex_t pointCount = static_cast<goIndex_t>(confMatrix.getRows());
    if (pointCount < 1)
    {
        return false;
    }

    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
    comRet.setSize (confMatrix.getColumns());
    comRet.fill (T(0));
    goIndex_t i = 0;
    goVector<T> ref;
    for (i = 0; i < pointCount; ++i)
    {
        confMatrix.refRow (i, ref);
        comRet += ref * factor;
    }
    return true;
}

template <class pointT>
bool goMath::centerOfMass (const goList<pointT>& points, pointT& comRet)
{
    if (points.isEmpty())
        return false;

    goIndex_t pointCount = static_cast<goIndex_t>(points.getSize());
    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
    //goDouble x = 0.0;
    //goDouble y = 0.0;
    //goDouble z = 0.0;
    //goDouble w = 0.0;
    pointT temp;
    temp.fill (0.0);
    typename goList<pointT>::ConstElement* el = points.getFrontElement();
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

template <class pointT>
bool goMath::centerOfMass (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, pointT& comRet)
{
    if (pointCount <= 0)
        return false;

    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
//    goDouble x = 0.0;
//    goDouble y = 0.0;
//    goDouble z = 0.0;
//    goDouble w = 0.0;
    pointT temp;
    temp.fill (0.0);
    typename goList<pointT>::ConstElement* el = begin;
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

/** 
 * @brief Translates all points in a configuration matrix.
 * 
 * @param confMatrix Configuration matrix, one point per row.
 * @param trans Translation vector.
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::translate (goMatrix<T>& confMatrix, const goVector<T>& trans)
{
    goVector<T> ref;
    goSize_t N = confMatrix.getRows();
    for (goSize_t i = 0; i < N; ++i)
    {
        confMatrix.refRow (i, ref);
        ref += trans;
    }
    return true;
}

template bool goMath::centerOfMass<goFloat> (const goList<goVector<goFloat> >&, goVector<goFloat>&);
template bool goMath::centerOfMass<goDouble> (const goList<goVector<goDouble> >&, goVector<goDouble>&);
template bool goMath::centerOfMass<goFloat> (const goMatrix<goFloat>&, goVector<goFloat>&);
template bool goMath::centerOfMass<goDouble> (const goMatrix<goDouble>&, goVector<goDouble>&);

template bool goMath::centerOfMass<goPointf> (const goList<goPointf>& points, goPointf& comRet);
template bool goMath::centerOfMass<goPointd> (const goList<goPointd>& points, goPointd& comRet);
template bool goMath::centerOfMass<goPointf> (goList<goPointf>::ConstElement* begin, goIndex_t pointCount, goPointf& comRet);
template bool goMath::centerOfMass<goPointd> (goList<goPointd>::ConstElement* begin, goIndex_t pointCount, goPointd& comRet);

template bool goMath::translate<goFloat> (goMatrix<goFloat>&, const goVector<goFloat>&);
template bool goMath::translate<goDouble> (goMatrix<goDouble>&, const goVector<goDouble>&);
