#include <gomath.h>

template <class T>
goMath::affineTransform<T>::affineTransform ()
    : A(), t()
{
}

template <class T>
goMath::affineTransform<T>::affineTransform (const goMatrix<T>& A_, const goVector<T>& t_)
    : A(A_), t(t_)
{
}

template <class T>
goMath::affineTransform<T>::~affineTransform ()
{
}

template <class T>
void goMath::affineTransform<T>::apply (goVector<T>& v)
{
    v = (A * v) + t;
}

template <class T>
void goMath::affineTransform<T>::apply (const goMatrix<T>& confMatrix, goMatrix<T>& ret)
{
    goMatrixMult<T> (T(1), confMatrix, false, this->A, true, T(0), ret);
    goSize_t sz = ret.getRows();
    goVector<T> ref;
    for (goSize_t i = 0; i < sz; ++i)
    {
        ret.refRow (i, ref);
        ref += this->t;
    }
}

template class goMath::affineTransform<goFloat>;
template class goMath::affineTransform<goDouble>;
