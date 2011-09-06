/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>

/** 
 * @brief Mean calculation.
 *
 * Slow implementation but prevents overflows.
 * 
 * @param v  Vector.
 * @param sz Number of elements in v.
 * 
 * @return Mean.
 */
template <class vectorT, class T>
T goMath::mean (const vectorT& v, goSize_t sz)
{
    goSize_t i;
    goDouble accum = 0.0;
    goDouble f = 1.0;
    for (i = 0; i < sz; ++i, f += 1.0)
    {
        accum = (accum * i + v[i]) / f;
    }
    return static_cast<T>(accum);
}

/** 
 * @brief Mean calculation.
 *
 * Fast implementation but can overflow (values are added, then divided once).
 * 
 * @param v  Vector.
 * @param sz Number of elements in v.
 * 
 * @return Mean.
 */
template <class vectorT, class T>
T goMath::fastMean (const vectorT& v, goSize_t sz)
{
    goSize_t i;
    goDouble accum = 0.0;
    for (i = 0; i < sz; ++i)
    {
        accum += v[i];
    }
    return static_cast<T>(accum / static_cast<goDouble>(sz));
}

template goDouble goMath::mean<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&, goSize_t sz);
template goDouble goMath::fastMean<goFixedArray<goDouble>,goDouble> (const goFixedArray<goDouble>&, goSize_t sz);
template goFloat  goMath::mean<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&, goSize_t sz);
template goFloat  goMath::fastMean<goFixedArray<goFloat>,goFloat> (const goFixedArray<goFloat>&, goSize_t sz);
