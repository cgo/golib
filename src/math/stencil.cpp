/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomathsignal.h>
#include <gosignal3dgenericiterator.h>

template <class T2, class T>
static T calculate_stencil (const goSignal3DBase<void>& sig, const goMath::Matrix<T>& s)
{
    goSignal3DGenericConstIterator it (&sig);

    goSize_t i = 0;
    T ret = T(0);
    while (!it.endY())
    {
        it.resetX();
        goSize_t j = 0;
        while (!it.endX())
        {
            ret += s(i,j) * *(T2*)*it;
            it.incrementX();
            ++j;
        }
        it.incrementY();
        ++i;
    }

    return ret;
}

/** \addtogroup math
 * @{
 */
/** 
 * @brief Calculate \f$ \sum_{i,j} sig(i,j,0) \cdot s(i,j) \f$.
 * 
 * Can be used to calculate a sum of a 2D stencil multiplied by the values in \c sig.
 * Only the z=0 plane is accounted for in \c sig.
 * You can for example have a \c goSubSignal3D move over a \c goSignal3DBase and
 * calculate some weighted sum (in 2D) at each point of interest.
 *
 * The sizes of sig and s should match.
 *
 * @param sig Signal
 * @param s   Matrix containing the stencil.
 * 
 * @return The sum of the elements in \c sig weighted with \c s.
 */
template <class T>
T goMath::stencil (const goSignal3DBase<void>& sig, const goMath::Matrix<T>& s)
{
    if (s.getRows() != sig.getSizeX() || s.getColumns() != sig.getSizeY())
    {
        goLog::warning ("goMath::stencil(): s and sig must match in size.");
        return 0.0;
    }
    switch (sig.getDataType().getID())
    {
        case GO_INT8: return calculate_stencil<goInt8, T> (sig, s); break;
        case GO_UINT8: return calculate_stencil<goUInt8, T> (sig, s); break;
        case GO_INT16: return calculate_stencil<goInt16, T> (sig, s); break;
        case GO_UINT16: return calculate_stencil<goUInt16, T> (sig, s); break;
        case GO_INT32: return calculate_stencil<goInt32, T> (sig, s); break;
        case GO_UINT32: return calculate_stencil<goUInt32, T> (sig, s); break;
        case GO_FLOAT: return calculate_stencil<goFloat, T> (sig, s); break;
        case GO_DOUBLE: return calculate_stencil<goDouble, T> (sig, s); break;
        default: goLog::warning ("goMath::stencil(): type error."); return 0.0; break;
    }
}
/** @} */

template goFloat goMath::stencil <goFloat> (const goSignal3DBase<void>&, const goMath::Matrix<goFloat>&);
template goDouble goMath::stencil <goDouble> (const goSignal3DBase<void>&, const goMath::Matrix<goDouble>&);
