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

template <class pointT>
bool goMath::centerOfMass (const goList<pointT>& points, pointT& comRet)
{
    if (points.isEmpty())
        return false;

    const pointT* p = 0;
    goIndex_t pointCount = static_cast<goIndex_t>(points.getSize());
    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
    goDouble x = 0.0;
    goDouble y = 0.0;
    goDouble z = 0.0;
    goDouble w = 0.0;
    typename goList<pointT>::ConstElement* el = points.getFrontElement();
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

template <class pointT>
bool goMath::centerOfMass (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, pointT& comRet)
{
    if (pointCount <= 0)
        return false;

    const pointT* p = 0;
    goDouble factor = 1.0 / static_cast<goDouble>(pointCount);
    goDouble x = 0.0;
    goDouble y = 0.0;
    goDouble z = 0.0;
    goDouble w = 0.0;
    typename goList<pointT>::ConstElement* el = begin;
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

template bool goMath::centerOfMass<goFloat> (const goList<goVector<goFloat> >&, goVector<goFloat>&);
template bool goMath::centerOfMass<goDouble> (const goList<goVector<goDouble> >&, goVector<goDouble>&);

template bool goMath::centerOfMass<goPointf> (const goList<goPointf>& points, goPointf& comRet);
template bool goMath::centerOfMass<goPointd> (const goList<goPointd>& points, goPointd& comRet);
template bool goMath::centerOfMass<goPointf> (goList<goPointf>::ConstElement* begin, goIndex_t pointCount, goPointf& comRet);
template bool goMath::centerOfMass<goPointd> (goList<goPointd>::ConstElement* begin, goIndex_t pointCount, goPointd& comRet);
