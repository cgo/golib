#include <gosignalhelper.h>
#include <gosignaloperation.h>


template <class T>
static inline T _min (T s1, T s2)
{
    return s1 < s2 ? s1 : s2;
}

template <class T>
static inline T _max (T s1, T s2)
{
    return s1 > s2 ? s1 : s2;
}

bool goSignalMax (const goSignal3DBase<void>& sig1, const goSignal3DBase<void>& sig2, goSignal3DBase<void>& target)
{
    if (sig1.getSize() != sig2.getSize() || sig1.getSize() != target.getSize())
    {
        return false;
    }

    goSignalOperation3<goDouble> op;
    op.setKernelMethod (goFunction<goDouble, goDouble, goDouble> (_max));
    return op (sig1, sig2, target);
}

bool goSignalMin (const goSignal3DBase<void>& sig1, const goSignal3DBase<void>& sig2, goSignal3DBase<void>& target)
{
    if (sig1.getSize() != sig2.getSize() || sig1.getSize() != target.getSize())
    {
        return false;
    }

    goSignalOperation3<goDouble> op;
    op.setKernelMethod (goFunction<goDouble, goDouble, goDouble> (_min));
    return op (sig1, sig2, target);
}
