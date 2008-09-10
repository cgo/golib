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
 * @addtogroup math
 * @{
 */
/** 
 * @brief Sehnentrapezformel. Step width (h) is assumed 1, so normalisation may be needed 
 * afterwards.
 * 
 * @param v Vector with function values at equidistant points.
 * 
 * @return Integral approximation.
 */
template <class T>
T goMath::integrate (const goMath::Vector<T>& v)
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

/** 
 * @brief Integration with Simpson rule. Step width h is assumed 1, so normalisation may be needed 
 * afterwards.
 *
 * The composite Simpson rule is
 * \f$ \int_a^b v(t) \, dt \approx \frac{h}{3} \left( v_0 + 2 \cdot \sum\limits_{j=1}^{n/2-1} v_{2j} + 4 \cdot \sum\limits_{j=1}^{n/2} v_{2j-1} + v_n\right) \f$, 
 * if the continuous function v(t) is divided 
 * into an even number n of equidistant intervals with distance h.
 * Note that \c integrateSimpson samples linearly between points given in \c v to obtain an even number of sampled points.
 * Also, only the division by 6 (not 3!) is done, since the step width h is assumed to be one. Normalisation
 * may therefore be done after calling this function by the user, e.g. if the interval [a,b] has length one, 
 * by multiplying with 1/v.getSize().
 *
 * @param v Vector with function values at equidistant points.
 * 
 * @return Integral approximation.
 */
template <class T>
T goMath::integrateSimpson (const goMath::Vector<T>& v)
{
    goSize_t sz = v.getSize();
    goSize_t szm1 = sz-1;
    
    T sum_2 = 0.0;
    T sum_4 = 0.0;
    for (goSize_t i = 0; i < szm1; ++i)
    {
        sum_4 += (v[i] + v[i+1]); // * 0.5 * 4.0;  ==> * 2.0
        sum_2 += v[i+1];  // * 2.0
    }
    sum_4 *= 2.0;
    sum_2 *= 2.0;

    return (v[0] + sum_2 + sum_4 + v[szm1]) / 6.0;   
}

template <class T>
T goMath::integrateSum (const goMath::Vector<T>& v)
{
    return v.sum ();
}
/** @} */

template goDouble goMath::integrate<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&,const goFixedArray<goDouble>&,goSize_t);
template goFloat goMath::integrate<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&,const goFixedArray<goFloat>&,goSize_t);
template goDouble goMath::integrate<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&,const goFixedArray<goDouble>&,goFixedArray<goDouble>&,goSize_t);
template goFloat goMath::integrate<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&,const goFixedArray<goFloat>&,goFixedArray<goFloat>&,goSize_t);

template goFloat goMath::integrate<goFloat> (const goMath::Vector<goFloat>&);
template goDouble goMath::integrate<goDouble> (const goMath::Vector<goDouble>&);
template goFloat goMath::integrateSimpson<goFloat> (const goMath::Vector<goFloat>&);
template goDouble goMath::integrateSimpson<goDouble> (const goMath::Vector<goDouble>&);
template goFloat goMath::integrateSum<goFloat> (const goMath::Vector<goFloat>&);
template goDouble goMath::integrateSum<goDouble> (const goMath::Vector<goDouble>&);
