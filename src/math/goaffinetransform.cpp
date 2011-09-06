/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>

template <class T>
goMath::AffineTransform<T>::AffineTransform ()
    : A(), t()
{
}

template <class T>
goMath::AffineTransform<T>::AffineTransform (const goMath::Matrix<T>& A_, const Vector<T>& t_)
    : A(A_), t(t_)
{
}

template <class T>
goMath::AffineTransform<T>::~AffineTransform ()
{
}

template <class T>
void goMath::AffineTransform<T>::apply (Vector<T>& v)
{
    v = (A * v) + t;
}

template <class T>
void goMath::AffineTransform<T>::apply (const goMath::Matrix<T>& confMatrix, goMath::Matrix<T>& ret)
{
    goMath::matrixMult<T> (T(1), confMatrix, false, this->A, true, T(0), ret);
    goSize_t sz = ret.getRows();
    Vector<T> ref;
    for (goSize_t i = 0; i < sz; ++i)
    {
        ret.refRow (i, ref);
        ref += this->t;
    }
}

template class goMath::AffineTransform<goFloat>;
template class goMath::AffineTransform<goDouble>;
