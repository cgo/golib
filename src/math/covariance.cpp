#include <gomath.h>
template <class T>
void goMath::covariance (const goMath::Matrix<T>& points, const Vector<T>& mean, goMath::Matrix<T>& ret)
{
    const Vector<T> x (0);
    ret.resize (0,0);
    goSize_t sz = points.getRows();
    T alpha = T(1) / T(sz);
    Vector<T> temp (0);
    for (goSize_t i = 0; i < sz; ++i)
    {
        temp = mean;
        points.refRow (i, x);
        vectorAdd<T> (T(-1), x, temp);
        vectorOuter<T> (alpha, temp, temp, ret);
    }
}

template void goMath::covariance<goFloat> (const goMath::Matrix<goFloat>& points, const Vector<goFloat>& mean, goMath::Matrix<goFloat>&);
template void goMath::covariance<goDouble> (const goMath::Matrix<goDouble>& points, const Vector<goDouble>& mean, goMath::Matrix<goDouble>&);
