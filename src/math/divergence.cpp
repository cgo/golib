#include <gomathsignal.h>
#include <golog.h>
#include <gosignal3dgenericiterator.h>
#include <gotype.h>

template <class T, class Tadd>
static bool addMasked2 (goSignal3DBase<void>& retValue, const goSignal3DBase<void>& add, const goSignal3DBase<void>* mask)
{
    goSignal3DGenericIterator it (&retValue);
    goSignal3DGenericConstIterator addIt (&add);
    goSignal3DGenericConstIterator maskIt (mask);

    while (!it.endZ())
    {
        it.resetY();
        addIt.resetY();
        maskIt.resetY();
        while (!it.endY())
        {
            it.resetX();
            addIt.resetX();
            maskIt.resetX();
            while (it.endX())
            {
                if (*(goInt8*)*maskIt != 0)
                {
                    *(T*)*it += *(Tadd*)*addIt;
                }
                it.incrementX();
                addIt.incrementX();
                maskIt.incrementX();
            }
            it.incrementY();
            addIt.incrementY();
            maskIt.incrementY();
        }
        it.incrementZ();
        addIt.incrementZ();
        maskIt.incrementZ();
    }
    return true;
}

template <class T>
static bool addMasked (goSignal3DBase<void>& retValue, const goSignal3DBase<void>& add, const goSignal3DBase<void>* mask)
{
    switch (add.getDataType().getID())
    {
        case GO_FLOAT: return addMasked2<T, goFloat> (retValue, add, mask); break;
        case GO_DOUBLE: return addMasked2<T, goDouble> (retValue, add, mask); break;
        default: goLog::warning ("addMasked(): illegal data type."); return false; break;
    }
    return false;
}

template <class T>
static bool divergence_ (const goSignal3DBase<void>& x, const goSignal3DBase<void>& y, goDouble hx, goDouble hy, goSignal3D<void>& retValue, const goSignal3DBase<void>* mask)
{
    // calculate central difference derivatives of x and y
    // and add them.
    goSignal3D<void> ddy_y;
    ddy_y.setDataType (y.getDataType().getID());
    if (!goMath::centralDifferences (x, retValue, 0, hx, mask))
        return false;
    if (!goMath::centralDifferences (y, ddy_y, 1, hy, mask))
        return false;
    if (!mask)
    {
        retValue += ddy_y;
    }
    else
    {
        switch (retValue.getDataType().getID())
        {
            case GO_FLOAT: addMasked<goFloat> (retValue, ddy_y, mask); break;
            case GO_DOUBLE: addMasked<goDouble> (retValue, ddy_y, mask); break;
            default: goLog::warning ("divergence_(): illegal type for retValue."); return false; break;
        }
    }
    return true;
}

/** 
 * @brief Calculate the divergence of a 2D vector field given by 2 goSignal3DBase.
 * 
 * Calculates \f$ D = \nabla V \f$, with V a two dimensional vector field over 
 * real numbers, using central differences. So 
 * \f$ D \approx \frac{\partial V}{\partial x} + \frac{\partial V}{\partial y} \f$
 *
 * The input must be of data type GO_FLOAT or GO_DOUBLE and
 * both components must be of the same data type and size.
 *
 * @param x X components of the vector field.
 * @param y Y components of the vector field.
 * @param hx  Grid spacing in X direction (set this to 1.0 for now).
 * @param hy  Grid spacing in Y direction (set this to 1.0 for now).
 * @param retValue  Divergence, will be allocated to the same data type 
 *   and size as the input data only if necessary.
 * @param mask Optional mask of type goInt8. Divergence is only calculated where mask != 0.
 *             All other values of retValue are not changed and undefined if retValue was not
 *             allocated correctly and filled before the call to this function.
 * 
 * @return True if successful, false otherwise.
 */
bool goMath::divergence (const goSignal3DBase<void>& x, const goSignal3DBase<void>& y, goDouble hx, goDouble hy, goSignal3D<void>& retValue, const goSignal3DBase<void>* mask)
{
    if (x.getDataType().getID() != y.getDataType().getID())
    {
        goLog::error ("goMath::divergence(): x and y must be of the same type.");
        return false;
    }
    if (x.getSizeX() != y.getSizeX() ||
        x.getSizeY() != y.getSizeY())
    {
        goLog::error ("goMath::divergence(): x and y must be of the same size.");
        return false;
    }
    if (retValue.getSize() != x.getSize() ||
        retValue.getDataType().getID() != x.getDataType().getID())
    {
        retValue.setDataType (x.getDataType().getID());
        goSize3D retBlockSize (goMath::min<goSize_t>(8,x.getSizeX()), 
                               goMath::min<goSize_t>(8,x.getSizeY()), 
                               goMath::min<goSize_t>(8,x.getSizeZ()));
        retValue.make (x.getSizeX(), x.getSizeY(), x.getSizeZ(),
                retBlockSize.x, retBlockSize.y, retBlockSize.z,
                4, 4, 4);
    }

    switch (x.getDataType().getID())
    {
        case GO_FLOAT: return divergence_<goFloat> (x,y,hx,hy,retValue,mask); break;
        case GO_DOUBLE: return divergence_<goDouble> (x,y,hx,hy,retValue,mask); break;
        default: 
            {
                goLog::error ("goMath::divergence(): Unsupported data type.");
                return false;
            }
    }
}
