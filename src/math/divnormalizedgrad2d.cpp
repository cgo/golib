#include <gomath.h>
#include <golog.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#include <gosignalhelper.h>

template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static inline bool divnormgrad2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                                   T dummyT = T(0), T_RET dummyTRET = T_RET(0));

template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static inline bool divnormgrad2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                                   T dummyT, T_RET dummyTRET)
{
    if (sig.getDataType().getID() != T_ENUM || retValue.getDataType().getID() != T_RET_ENUM)
    {
        goLog::warning ("goMath::divNormalizedGrad(): One of the arguments is not of the expected type.");
        return false;
    }

    // FIXME: Currently only works for 2D
    goSignal3D<void> gradient;
    gradient.setDataType (T_RET_ENUM);
    gradient.make (sig.getSizeX(), sig.getSizeY(), 2,
                   sig.getBlockSizeX(), sig.getBlockSizeY(), sig.getBlockSizeZ(),
                   16, 16, 16);
    if (!goMath::gradient2D (sig, gradient))
    {
        return false;
    }

    // Normalize gradient
    goByte* p1      = NULL;
    goByte* p2      = NULL;
    goPtrdiff_t* dx = NULL;
    goPtrdiff_t dz  = *gradient.getZDiff();
    goSize_t i;
    goSize_t j;
    goFloat normFactor = 1.0f;
    for (j = 0; j < gradient.getSizeY(); ++j)
    {
        p1 = (goByte*)gradient.getPtr (0,j,0);
        p2 = p1 + dz;
        dx = gradient.getXDiff();
        for (i = 0; i < gradient.getSizeX(); ++i)
        {
            normFactor = sqrt(*(T_RET*)p1 * *(T_RET*)p1 + *(T_RET*)p2 * *(T_RET*)p2);
            if (normFactor < goMath::epsilon)
            {
                normFactor = 0.0f;
            }
            else
            {
                normFactor = 1.0f / normFactor;
            }
            *(T_RET*)p1 *= normFactor;
            *(T_RET*)p2 *= normFactor;
            p1 += *dx;
            p2 += *dx;
            ++dx;
        }
    }
    
    goSubSignal3D<void> gradX (&gradient, gradient.getSizeX(), gradient.getSizeY(), 1);
    goSubSignal3D<void> gradY (&gradient, gradient.getSizeX(), gradient.getSizeY(), 1);
    gradY.setPosition (0, 0, 1);
    goSignal3D<void> temp;
    temp.setDataType (T_RET_ENUM);
    temp.make (gradient.getSizeX(), gradient.getSizeY(), 1, 
               gradient.getBlockSizeX(), gradient.getBlockSizeY(), gradient.getBlockSizeZ(),
               16, 16, 1);
    if (!goMath::ddx2D (gradX, temp))
    {
        goLog::warning ("goMath::divNormalizedGrad(): Could not calculate ddx.");
        return false;
    }
    if (!goMath::ddy2D (gradY, retValue))
    {
        goLog::warning ("goMath::divNormalizedGrad(): Could not calculate ddx.");
        return false;
    }
    GO_SIGNAL3D_EACHELEMENT_2_GENERIC (*(T_RET*)__ptr_target += *(T_RET*)__ptr, temp, retValue);
    temp.destroy();
    gradient.destroy();
    return true;
}

/**
 * @brief Calculates div(grad(sig)/|grad(sig)|) of a 2D (!) signal.
 *
 * Should be working with any combination of input and output signals (of the same size!) 
 * of types float and double.
 * Only 2D signals, so the z-size of the input and output signals must be 1.
 * 
 * @param sig       Input signal, type must be GO_FLOAT or GO_DOUBLE.
 * @param retValue  Return value, must be of the same size as the input signal and
 *                  of type GO_FLOAT or GO_DOUBLE.
 *
 * @return True if successful, false otherwise.
 **/
bool goMath::divNormalizedGrad2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue)
{
    switch (sig.getDataType().getID())
    {
        case GO_FLOAT:
            {
                switch (retValue.getDataType().getID())
                {
                    case GO_FLOAT:
                        {
                            return divnormgrad2D_<goFloat, GO_FLOAT, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return divnormgrad2D_<goFloat, GO_FLOAT, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::divnormgrad2D(): Type combination ";
                            msg += "<"; msg += sig.getDataType().getString().toCharPtr();
                            msg += ","; msg += retValue.getDataType().getString().toCharPtr();
                            msg += "> not supported.";
                            goLog::warning(msg);
                            return false;
                        }
                        break;
                }
            }
            break;
        case GO_DOUBLE:
            {
                switch (retValue.getDataType().getID())
                {
                    case GO_FLOAT:
                        {
                            return divnormgrad2D_<goDouble, GO_DOUBLE, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return divnormgrad2D_<goDouble, GO_DOUBLE, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::divnormgrad2D(): Type combination ";
                            msg += "<"; msg += sig.getDataType().getString().toCharPtr();
                            msg += ","; msg += retValue.getDataType().getString().toCharPtr();
                            msg += "> not supported.";
                            goLog::warning(msg);
                            return false;
                        }
                        break;
                }
            }
            break;
        default:
            {
                goString msg;
                msg = "goMath::divnormgrad2D(): Input type";
                msg += "'"; msg += sig.getDataType().getString().toCharPtr();
                msg += "' not supported."; 
                goLog::warning(msg);
                return false;
            }
            break;
    }
    return false;    
}
