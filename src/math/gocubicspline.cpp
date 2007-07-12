#include <gomath.h>
#include <gocubicspline.h>

template<class T>
goMath::CubicSpline<T>::CubicSpline ()
   : myA_inv(0,0), myM(4,4)
{
    static T A_inv[] = {T(2), T(-3), T(0), T(1),
                        T(-2), T(3), T(0), T(0),
                        T(1), T(-2), T(1), T(0),
                        T(1), T(-1), T(0), T(0)};
    myA_inv.setData (A_inv, 4, 4, 4);
}

template<class T>
goMath::CubicSpline<T>::CubicSpline (const goMatrix<T>& points)
   : myA_inv(0,0), myM(4,4)
{
    static T A_inv[] = {T(2), T(-3), T(0), T(1),
                        T(-2), T(3), T(0), T(0),
                        T(1), T(-2), T(1), T(0),
                        T(1), T(-1), T(0), T(0)};
    myA_inv.setData (A_inv, 4, 4, 4);
    this->fit (points);
}

template<class T>
goMath::CubicSpline<T>::CubicSpline (const goVector<T>& pm1,
        const goVector<T>& p0,
        const goVector<T>& p1,
        const goVector<T>& p2)
   : myA_inv(0,0), myM(4,4)
{
    static T A_inv[] = {T(2), T(-3), T(0), T(1),
                        T(-2), T(3), T(0), T(0),
                        T(1), T(-2), T(1), T(0),
                        T(1), T(-1), T(0), T(0)};
    myA_inv.setData (A_inv, 4, 4, 4);
    this->fit (pm1, p0, p1, p2);
}

template<class T>
goMath::CubicSpline<T>::~CubicSpline ()
{
}

template<class T>
goAutoPtr<goVector<T> > 
goMath::CubicSpline<T>::operator() (T t) const
{
    goAutoPtr<goVector<T> > ret = goAutoPtr<goVector<T> >(new goVector<T>(2));
    this->eval (t, *ret);
    return ret;
}

template<class T>
bool goMath::CubicSpline<T>::eval (T t, goVector<T>& ret) const
{
    if (ret.getSize() != 2)
        ret.resize(2);
   
    //= Calculate explicitly to save some operations.
    ret[0] = t * (t * (t * this->myM(0,0) + this->myM(0,1)) + this->myM(0,2)) + this->myM(0,3);
    ret[1] = t * (t * (t * this->myM(1,0) + this->myM(1,1)) + this->myM(1,2)) + this->myM(1,3);

    return true;
}

template<class T>
bool goMath::CubicSpline<T>::D (T t, goVector<T>& ret) const
{
    if (ret.getSize() != 2)
        ret.resize(2);
   
    //= Calculate explicitly to save some operations.
    ret[0] = t * (t * 3.0 * this->myM(0,0) + 2.0 * this->myM(0,1)) + this->myM(0,2);
    ret[1] = t * (t * 3.0 * this->myM(1,0) + 2.0 * this->myM(1,1)) + this->myM(1,2);

    return true;
}

template<class T>
bool goMath::CubicSpline<T>::fit (const goMatrix<T>& points)
{
    goVector<T> pm1, p0, p1, p2;
    points.refRow(0,pm1);
    points.refRow(1,p0);
    points.refRow(2,p1);
    points.refRow(3,p2);
    return this->fit (pm1, p0, p1, p2);
}

template<class T>
bool goMath::CubicSpline<T>::fit (const goVector<T>& pm1,
        const goVector<T>& p0,
        const goVector<T>& p1,
        const goVector<T>& p2)
{
    static goMatrix<T> P (2,4);
    P.setColumn(0,p0);
    P.setColumn(1,p1);
    P.setColumn(2,(p1-pm1)*0.5);
    P.setColumn(3,(p2-p0)*0.5);
    
    goMatrixMult<T> (T(1), P, false, this->myA_inv, false, T(0), this->myM);
    return true;
}

template class goMath::CubicSpline<goFloat>;
template class goMath::CubicSpline<goDouble>;
