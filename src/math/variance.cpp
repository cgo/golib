#include <gomath.h>

template <class vectorT, class T>
T goMath::variance (const vectorT& v, goSize_t sz, T mean)
{
    goDouble accum = 0.0;
    goSize_t i;
    goDouble temp;
    goDouble f = 1.0;
    for (i = 0; i < sz; ++i, f += 1.0)
    {
        temp = v[i] - mean;
        accum = (accum * T(i) + temp * temp) / f;
    }
    return static_cast<T>(accum);
}

template <class vectorT, class T>
T goMath::fastVariance (const vectorT& v, goSize_t sz, T mean)
{
    goDouble accum = 0.0;
    goSize_t i;
    goDouble temp;
    for (i = 0; i < sz; ++i)
    {
        temp = v[i] - mean;
        accum += temp * temp;
    }
    return static_cast<T>(accum / static_cast<goDouble>(sz - 1));
}

template goDouble goMath::variance<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&, goSize_t sz, goDouble mean);
template goDouble goMath::fastVariance<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&, goSize_t sz, goDouble mean);
template goDouble goMath::variance<goMath::Vectord,goDouble> (const goMath::Vectord&, goSize_t sz, goDouble mean);
template goDouble goMath::fastVariance<goMath::Vectord,goDouble> (const goMath::Vectord&, goSize_t sz, goDouble mean);
template goFloat  goMath::variance<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&, goSize_t sz, goFloat mean);
template goFloat  goMath::fastVariance<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&, goSize_t sz, goFloat mean);
