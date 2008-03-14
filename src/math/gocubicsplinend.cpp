#include <gomath.h>
#include <gocubicsplinend.h>

template<class T>
goMath::CubicSplineND<T>::CubicSplineND ()
   : myA_inv(0,0), myM(4,4)
{
    static T A_inv[] = {T(2), T(-3), T(0), T(1),
                        T(-2), T(3), T(0), T(0),
                        T(1), T(-2), T(1), T(0),
                        T(1), T(-1), T(0), T(0)};
    myA_inv.setData (A_inv, 4, 4, 4);
}

template<class T>
goMath::CubicSplineND<T>::CubicSplineND (const goMatrix<T>& points)
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
goMath::CubicSplineND<T>::CubicSplineND (const goVector<T>& pm1,
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
goMath::CubicSplineND<T>::~CubicSplineND ()
{
}

template<class T>
goAutoPtr<goVector<T> > 
goMath::CubicSplineND<T>::operator() (T t) const
{
    goAutoPtr<goVector<T> > ret = goAutoPtr<goVector<T> >(new goVector<T>(this->myM.getRows()));
    this->eval (t, *ret);
    return ret;
}

template<class T>
bool goMath::CubicSplineND<T>::eval (T t, goVector<T>& ret) const
{
    T tt = t*t;
    T ttt = tt * t;
    T _t[] = {ttt, tt, t, T(1)};
    goVector<T> tvec (_t, 4, 1);

    ret = this->myM * tvec;
    return true;
}

template<class T>
bool goMath::CubicSplineND<T>::D (T t, goVector<T>& ret) const
{
    T _t[] = {t*t, t, T(1), T(0)};
    goVector<T> tvec (_t, 4, 1);

    ret = this->myM * tvec;

    return true;
}

template<class T>
bool goMath::CubicSplineND<T>::fit (const goMatrix<T>& points)
{
    goVector<T> pm1, p0, p1, p2;
    points.refRow(0,pm1);
    points.refRow(1,p0);
    points.refRow(2,p1);
    points.refRow(3,p2);
    return this->fit (pm1, p0, p1, p2);
}

template<class T>
bool goMath::CubicSplineND<T>::fit (const goVector<T>& pm1,
        const goVector<T>& p0,
        const goVector<T>& p1,
        const goVector<T>& p2)
{
    static goMatrix<T> P (p0.getSize(), 4);
    P.setColumn(0,p0);
    P.setColumn(1,p1);
    P.setColumn(2,(p1-pm1)*0.5);
    P.setColumn(3,(p2-p0)*0.5);
    
    goMatrixMult<T> (T(1), P, false, this->myA_inv, false, T(0), this->myM);
    return true;
}

template class goMath::CubicSplineND<goFloat>;
template class goMath::CubicSplineND<goDouble>;
