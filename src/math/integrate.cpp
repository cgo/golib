#include <gomath.h>

template <class vectorT, class T>
T goMath::integrate (const vectorT& x, const vectorT& y, goSize_t sz)
{
    goDouble I = 0.0;
    goSize_t i;
    //= Sehnentrapezformel
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
    //= Sehnentrapezformel
    for (i = 1; i < sz; ++i)
    {
        I += (y[i] + y[i-1]) * 0.5 * (x[i] - x[i-1]);
        ret[i] = I;
    }
    return static_cast<T>(I);
}

/** 
 * @brief Sehnentrapezformel. Step width is assume 1, so normalisation may be needed 
 * afterwards.
 * 
 * @param v Vector with function values at equidistant points.
 * 
 * @return Integral approximation.
 */
template <class T>
T goMath::integrate (const goVector<T>& v)
{
    goSize_t sz = v.getSize();
    goSize_t szm1 = sz-1;
    T ret = 0.5 * (v[0] + v[szm1]);
    for (goSize_t i = 1; i < szm1; ++i)
    {
        ret += v[i];
    }
    return ret;
}

template goDouble goMath::integrate<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&,const goFixedArray<goDouble>&,goSize_t);
template goFloat goMath::integrate<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&,const goFixedArray<goFloat>&,goSize_t);
template goDouble goMath::integrate<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&,const goFixedArray<goDouble>&,goFixedArray<goDouble>&,goSize_t);
template goFloat goMath::integrate<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&,const goFixedArray<goFloat>&,goFixedArray<goFloat>&,goSize_t);

template goFloat goMath::integrate<goFloat> (const goVector<goFloat>&);
template goDouble goMath::integrate<goDouble> (const goVector<goDouble>&);
