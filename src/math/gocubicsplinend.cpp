/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
goMath::CubicSplineND<T>::CubicSplineND (const goMath::Matrix<T>& points)
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
goMath::CubicSplineND<T>::CubicSplineND (const goMath::Vector<T>& pm1,
        const goMath::Vector<T>& p0,
        const goMath::Vector<T>& p1,
        const goMath::Vector<T>& p2)
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
goAutoPtr<goMath::Vector<T> > 
goMath::CubicSplineND<T>::operator() (T t) const
{
    goAutoPtr<goMath::Vector<T> > ret = goAutoPtr<goMath::Vector<T> >(new goMath::Vector<T>(this->myM.getRows()));
    this->eval (t, *ret);
    return ret;
}

template<class T>
bool goMath::CubicSplineND<T>::eval (T t, goMath::Vector<T>& ret) const
{
    T tt = t*t;
    T ttt = tt * t;
    T _t[] = {ttt, tt, t, T(1)};
    goMath::Vector<T> tvec (_t, 4, 1);

    ret = this->myM * tvec;
    return true;
}

template<class T>
bool goMath::CubicSplineND<T>::D (T t, goMath::Vector<T>& ret) const
{
    T _t[] = {t*t, t, T(1), T(0)};
    goMath::Vector<T> tvec (_t, 4, 1);

    ret = this->myM * tvec;

    return true;
}

template<class T>
bool goMath::CubicSplineND<T>::fit (const goMath::Matrix<T>& points)
{
    goMath::Vector<T> pm1, p0, p1, p2;
    points.refRow(0,pm1);
    points.refRow(1,p0);
    points.refRow(2,p1);
    points.refRow(3,p2);
    return this->fit (pm1, p0, p1, p2);
}

template<class T>
bool goMath::CubicSplineND<T>::fit (const goMath::Vector<T>& pm1,
        const goMath::Vector<T>& p0,
        const goMath::Vector<T>& p1,
        const goMath::Vector<T>& p2)
{
    static goMath::Matrix<T> P (p0.getSize(), 4);
    P.setColumn(0,p0);
    P.setColumn(1,p1);
    P.setColumn(2,(p1-pm1)*0.5);
    P.setColumn(3,(p2-p0)*0.5);
    
    goMath::matrixMult<T> (T(1), P, false, this->myA_inv, false, T(0), this->myM);
    return true;
}

template class goMath::CubicSplineND<goFloat>;
template class goMath::CubicSplineND<goDouble>;
