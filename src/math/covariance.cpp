#include <gomath.h>
template <class T>
void goMath::covariance (const goMatrix<T>& points, const goVector<T>& mean, goMatrix<T>& ret)
{
    const goVector<T> x (0);
    ret.resize (0,0);
    goSize_t sz = points.getRows();
    T alpha = T(1) / T(sz);
    goVector<T> temp (0);
    for (goSize_t i = 0; i < sz; ++i)
    {
        temp = mean;
        points.refRow (i, x);
        goVectorAdd<T> (T(-1), x, temp);
        goVectorOuter<T> (alpha, temp, temp, ret);
    }
}

template void goMath::covariance<goFloat> (const goMatrix<goFloat>& points, const goVector<goFloat>& mean, goMatrix<goFloat>&);
template void goMath::covariance<goDouble> (const goMatrix<goDouble>& points, const goVector<goDouble>& mean, goMatrix<goDouble>&);
