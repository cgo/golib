#include <gomath.h>

template <class vectorT, class T>
T goMath::integrate (const vectorT& x, const vectorT& y, goSize_t sz)
{
    goDouble I = 0.0;
    goSize_t i;
    for (i = 1; i < sz; ++i)
    {
        I += (y[i] + y[i-1]) * 0.5 * (x[i] - x[i-1]);
    }
    return static_cast<T>(I);
}

template <class vectorT, class T>
T goMath::integrate (const vectorT& x, const vectorT& y, vectorT& ret, goSize_t sz)
{
    goDouble I = 0.0;
    goSize_t i;
    ret[0] = I;
    for (i = 1; i < sz; ++i)
    {
        I += (y[i] + y[i-1]) * 0.5 * (x[i] - x[i-1]);
        ret[i] = I;
    }
    return static_cast<T>(I);
}

template goDouble goMath::integrate<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&,const goFixedArray<goDouble>&,goSize_t);
template goFloat goMath::integrate<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&,const goFixedArray<goFloat>&,goSize_t);
template goDouble goMath::integrate<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&,const goFixedArray<goDouble>&,goFixedArray<goDouble>&,goSize_t);
template goFloat goMath::integrate<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&,const goFixedArray<goFloat>&,goFixedArray<goFloat>&,goSize_t);
