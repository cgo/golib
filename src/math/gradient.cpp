#ifndef GOMATH_H
# include <gomath.h>
#endif
#include <gosignal3dbase.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>
#include <gostring.h>

template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static inline bool laplacian2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                                 T dummyT = T(0), T_RET dummyTRET = T_RET(0));
template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static inline bool gradient2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                                T dummyT = T(0), T_RET dummyTRET = T_RET(0));
template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static inline bool ddx2D_(const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                          T dummyT = T(0), T_RET dummyTRET = T_RET(0));
template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static inline bool ddy2D_(const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                          T dummyT = T(0), T_RET dummyTRET = T_RET(0));

template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static bool laplacian2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                          T dummyT, T_RET dummyTRET) 
{
    if (sig.getDataType().getID() != T_ENUM || retValue.getDataType().getID() != T_RET_ENUM)
    {
        goString msg = "goMath::laplacian2D: signal and return value are not of the expected type, instead they are ";
        msg += sig.getDataType().getString().toCharPtr(); 
        msg += " and "; 
        msg += retValue.getDataType().getString().toCharPtr();
        goLog::warning (msg);
        return false;
    }
    if (retValue.getSizeX() != sig.getSizeX() ||
        retValue.getSizeY() != sig.getSizeY() ||
        retValue.getSizeZ() != 1)
    {
        goString msg = "goMath::laplacian2D: return signal size not correct.";
        msg += "Size is ";
        msg += (int)retValue.getSizeX(); 
        msg += " "; msg += (int)retValue.getSizeY(); 
        msg += " "; msg += (int)retValue.getSizeZ();
        msg += " but should be "; (int)sig.getSizeX(); 
        msg += " "; msg += (int)sig.getSizeY(); msg += " 1";
        goLog::warning (msg);
        return false;
    }
    
    const goPtrdiff_t* sigDx = NULL;
    const goPtrdiff_t* sigDy = sig.getYDiff();
    goPtrdiff_t* retDx = NULL;
    goPtrdiff_t* retDy = retValue.getYDiff();
    const goByte* sigP;
    goByte*       retP;

    goSize_t i,j;
    // goFloat u[9];
    goFloat lambda = 1.0f/3.0f;
    goFloat _lambda = 1.0f - lambda;
    for (j = 0; j < sig.getSizeY(); ++j)
    {
        sigP = (const goByte*)sig.getPtr(0,j,0);
        retP = (goByte*)retValue.getPtr(0,j,0);
        sigDx = sig.getXDiff();
        retDx = retValue.getXDiff();
        for (i = 0; i < sig.getSizeX(); ++i)
        {
            *(T_RET*)retP = lambda * (*(T*)(sigP + *sigDx) + 
                                      *(T*)(sigP - *(sigDx-1)) + 
                                      *(T*)(sigP + *sigDy) + 
                                      *(T*)(sigP - *(sigDy-1)) - 
                                        4**(T*)sigP) + 
                            _lambda * (*(T*)(sigP + *sigDx + *sigDy) + 
                                        *(T*)(sigP - *(sigDx-1) + *sigDy) + 
                                        *(T*)(sigP + *sigDx - *(sigDy-1)) + 
                                        *(T*)(sigP - *(sigDx-1) - 
                                        *(sigDy-1)) - 4**(T*)sigP) * 0.5f;

            retP += *retDx;
            sigP += *sigDx;
            ++retDx;
            ++sigDx;
        }
        ++retDy;
        ++sigDy;
    }
    return true;

}

template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static bool gradient2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                          T dummyT, T_RET dummyTRET) 
{
    if (sig.getDataType().getID() != T_ENUM || retValue.getDataType().getID() != T_RET_ENUM)
    {
        goString msg = "goMath::gradient2D: signal or return value are not of the expected type, instead they are ";
        msg += sig.getDataType().getString().toCharPtr(); msg += " and "; msg += retValue.getDataType().getString().toCharPtr();
        goLog::warning (msg);
        return false;
    }
    if (retValue.getSizeX() != sig.getSizeX() ||
        retValue.getSizeY() != sig.getSizeY() ||
        retValue.getChannelCount() != 2)
    {
        goString msg = "goMath::gradient2D: return signal size not correct.";
        msg += "Size is ";
        msg += (int)retValue.getSizeX(); msg += " "; msg += (int)retValue.getSizeY(); msg += " "; msg += (int)retValue.getSizeZ();
        msg += " but should be "; (int)sig.getSizeX(); msg += " "; msg += (int)sig.getSizeY(); msg += " 1 and 2 channels";
        goLog::warning (msg);
        return false;
    }

    retValue.setChannel(0);
    const goPtrdiff_t* sigDx = NULL;
    const goPtrdiff_t* sigDy = sig.getYDiff();
    goPtrdiff_t*  retDx = NULL;
    goPtrdiff_t*  retDy = retValue.getYDiff();
    const goByte* sigP = 0;
    goByte*       retP1 = 0;

    goSize_t i,j;
    goFloat lambda = sqrt(2.0f) - 1.0f;
    goFloat _lambda = (1.0f - lambda) * 0.5f;
    for (j = 0; j < sig.getSizeY(); ++j)
    {
        sigP  = (const goByte*)sig.getPtr(0,j,0);
        retP1 = (goByte*)retValue.getPtr(0,j,0);
        sigDx = sig.getXDiff();
        retDx = retValue.getXDiff();
        for (i = 0; i < sig.getSizeX(); ++i)
        {
            *(T_RET*)retP1 = lambda * (*(T*)(sigP + *sigDx) - 
                                         *(T*)(sigP - *(sigDx-1))) + 
                               _lambda * (*(T*)(sigP + *sigDx + *sigDy) - 
                                          *(T*)(sigP - *(sigDx-1) + *sigDy) + 
                                          *(T*)(sigP + *sigDx - *(sigDy-1)) - 
                                          *(T*)(sigP - *(sigDx-1) - *(sigDy-1))) * 0.5f;
            *((T_RET*)retP1 + 1) = lambda * (*(T*)(sigP + *sigDy) - 
                                         *(T*)(sigP - *(sigDy-1))) + 
                               _lambda * (*(T*)(sigP + *sigDx + *sigDy) - 
                                          *(T*)(sigP - *(sigDy-1) + *sigDx) + 
                                          *(T*)(sigP + *sigDy - *(sigDx-1)) - 
                                          *(T*)(sigP - *(sigDx-1) - *(sigDy-1))) * 0.5f;
            retP1 += *retDx;
            sigP += *sigDx;
            ++retDx;
            ++sigDx;
        }
        ++retDy;
        ++sigDy;
    }
    return true;
}

template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static bool ddx2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                    T dummyT, T_RET dummyTRET)
{
    if (sig.getDataType().getID() != T_ENUM || retValue.getDataType().getID() != T_RET_ENUM)
    {
        goString msg = "goMath::ddx2D: signal or return value ae not of the expected type, instead they are ";
        msg += sig.getDataType().getString().toCharPtr(); 
        msg += " and "; 
        msg += retValue.getDataType().getString().toCharPtr();
        goLog::warning (msg);
        return false;
    }
    if (retValue.getSizeX() != sig.getSizeX() ||
        retValue.getSizeY() != sig.getSizeY() ||
        retValue.getSizeZ() != 1)
    {
        goString msg = "goMath::ddx2D: return signal size not correct.";
        msg += "Size is ";
        msg += (int)retValue.getSizeX(); msg += " "; msg += (int)retValue.getSizeY(); msg += " "; msg += (int)retValue.getSizeZ();
        msg += " but should be "; (int)sig.getSizeX(); msg += " "; msg += (int)sig.getSizeY(); msg += " 1";
        goLog::warning (msg);
        return false;
    }

    const goPtrdiff_t* sigDx = NULL;
    const goPtrdiff_t* sigDy = sig.getYDiff();
    goPtrdiff_t*  retDx = NULL;
    goPtrdiff_t*  retDy = retValue.getYDiff();
    const goByte* sigP;
    goByte*       retP;

    goSize_t i,j;
    // goFloat u[9];
    goFloat lambda = sqrt(2.0f) - 1.0f;
    goFloat _lambda = (1.0f - lambda) * 0.5f;
    for (j = 0; j < sig.getSizeY(); ++j)
    {
        sigP  = (const goByte*)sig.getPtr(0,j,0);
        retP  = (goByte*)retValue.getPtr(0,j,0);
        sigDx = sig.getXDiff();
        retDx = retValue.getXDiff();
        for (i = 0; i < sig.getSizeX(); ++i)
        {
            *(T_RET*)retP = lambda * (*(T*)(sigP + *sigDx) - 
                                      *(T*)(sigP - *(sigDx-1))) + 
                              _lambda * (*(T*)(sigP + *sigDx + *sigDy) - 
                                         *(T*)(sigP - *(sigDx-1) + *sigDy) + 
                                         *(T*)(sigP + *sigDx - *(sigDy-1)) - 
                                         *(T*)(sigP - *(sigDx-1) - *(sigDy-1))) * 0.5f;
            retP += *retDx;
            sigP += *sigDx;
            ++retDx;
            ++sigDx;
        }
        ++retDy;
        ++sigDy;
    }
    return true;
}

template <class T, goTypeEnum T_ENUM, class T_RET, goTypeEnum T_RET_ENUM>
static bool ddy2D_ (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue, 
                    T dummyT, T_RET dummyTRET)
{
    if (sig.getDataType().getID() != T_ENUM || retValue.getDataType().getID() != T_RET_ENUM)
    {
        goString msg = "goMath::ddx2D: signal or return value are not of the expected type, instead they are ";
        msg += sig.getDataType().getString().toCharPtr(); 
        msg += " and "; 
        msg += retValue.getDataType().getString().toCharPtr();
        goLog::warning (msg);
        return false;
    }
    if (retValue.getSizeX() != sig.getSizeX() ||
        retValue.getSizeY() != sig.getSizeY() ||
        retValue.getSizeZ() != 1)
    {
        goString msg = "goMath::ddx2D: return signal size not correct.";
        msg += "Size is ";
        msg += (int)retValue.getSizeX(); msg += " "; msg += (int)retValue.getSizeY(); msg += " "; msg += (int)retValue.getSizeZ();
        msg += " but should be "; msg += (int)sig.getSizeX(); msg += " "; msg += (int)sig.getSizeY(); msg += " 1";
        goLog::warning (msg);
        return false;
    }

    const goPtrdiff_t* sigDx = NULL;
    const goPtrdiff_t* sigDy = sig.getYDiff();
    goPtrdiff_t*  retDx = NULL;
    goPtrdiff_t*  retDy = retValue.getYDiff();
    const goByte* sigP;
    goByte*       retP;

    goSize_t i,j;
    // goFloat u[9];
    goFloat lambda = sqrt(2.0f) - 1.0f;
    goFloat _lambda = (1.0f - lambda) * 0.5f;
    for (j = 0; j < sig.getSizeY(); ++j)
    {
        sigP  = (const goByte*)sig.getPtr(0,j,0);
        retP  = (goByte*)retValue.getPtr(0,j,0);
        sigDx = sig.getXDiff();
        retDx = retValue.getXDiff();
        for (i = 0; i < sig.getSizeX(); ++i)
        {
            *(T_RET*)retP = lambda * (*(T*)(sigP + *sigDy) - 
                                        *(T*)(sigP - *(sigDy-1))) + 
                              _lambda * (*(T*)(sigP + *sigDx + *sigDy) - 
                                         *(T*)(sigP - *(sigDy-1) + *sigDx) + 
                                         *(T*)(sigP + *sigDy - *(sigDx-1)) - 
                                         *(T*)(sigP - *(sigDx-1) - *(sigDy-1))) * 0.5f;
            retP += *retDx;
            sigP += *sigDx;
            ++retDx;
            ++sigDx;
        }
        ++retDy;
        ++sigDy;
    }
    return true;
}
/**
* @addtogroup math
* @{
*/
/**
 * @brief Calculates the laplacian (2nd derivative) of a 2D signal (z-size == 1).
 *                  
 * @note  The data type of the arguments is currently restricted to GO_FLOAT,
 *        but GO_DOUBLE will also be implemented.
 *        Multichannel data is not yet supported by libGo.
 *
 * @param sig       Contains the 2D signal.
 * @param retValue  After returning true, retValue contains the 
 *                  laplace(sig).
 *                  retValue must be of the same size as 
 *                  sig in x and y dimensions and its z dimension must
 *                  be 1.
 *
 * \todo BUG: This sometimes can produce NaNs.
 * 
 * @return True if successful, false otherwise.
 **/
bool goMath::laplacian2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue)
{
    switch (sig.getDataType().getID())
    {
        case GO_FLOAT:
            {
                switch (retValue.getDataType().getID())
                {
                    case GO_FLOAT:
                        {
                            return laplacian2D_<goFloat, GO_FLOAT, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return laplacian2D_<goFloat, GO_FLOAT, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::laplacian2D(): Type combination ";
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
                            return laplacian2D_<goDouble, GO_DOUBLE, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return laplacian2D_<goDouble, GO_DOUBLE, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::laplacian2D(): Type combination ";
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
                msg = "goMath::laplacian2D(): Input type";
                msg += "'"; msg += sig.getDataType().getString().toCharPtr();
                msg += "' not supported."; 
                goLog::warning(msg);
                return false;
            }
            break;
    }
    return false;    
}

/**
 * @brief Calculates the gradient of a 2D signal (z-size == 1).
 *                  
 * @note  The data type of the arguments is currently restricted to GO_FLOAT and
 *        GO_DOUBLE.
 *
 * @todo Look at the code and find the reference where this is documented.
 *       It's not simply central differences, but rather central differences 
 *       including the next neighbours.
 * 
 * @param sig       Contains the 2D signal.
 * @param retValue  After returning true, retValue contains the 
 *                  x and y components of grad(sig).
 *                  retValue must be of the same size as 
 *                  sig in x and y dimensions and its channel-count must
 *                  be 2.
 *
 * @return True if successful, false otherwise.
 **/
bool goMath::gradient2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue)
{
    switch (sig.getDataType().getID())
    {
        case GO_FLOAT:
            {
                switch (retValue.getDataType().getID())
                {
                    case GO_FLOAT:
                        {
                            return gradient2D_<goFloat, GO_FLOAT, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return gradient2D_<goFloat, GO_FLOAT, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::gradient2D(): Type combination ";
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
                            return gradient2D_<goDouble, GO_DOUBLE, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return gradient2D_<goDouble, GO_DOUBLE, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::gradient2D(): Type combination ";
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
                msg = "goMath::gradient2D(): Input type";
                msg += "'"; msg += sig.getDataType().getString().toCharPtr();
                msg += "' not supported."; 
                goLog::warning(msg);
                return false;
            }
            break;
    }
    return false;    
}

/**
* @brief  Calculates the derivative in x direction assuming a 2D signal.
*
* This function calculates the derivative in x-direction while assuming
* the signal is 2-dimensional.
* The arguments are restricted to signals of type GO_FLOAT and
* GO_DOUBLE. 
*
* @todo Implement for 3D-signals.
* 
* @param sig       Signal to calculate the derivative of.
* @param retValue  After the function returned true, contains
*                  the x-derivative of sig. Must be of the same
*                  size as sig.
*
* @return 
**/
bool goMath::ddx2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue)
{
    switch (sig.getDataType().getID())
    {
        case GO_FLOAT:
            {
                switch (retValue.getDataType().getID())
                {
                    case GO_FLOAT:
                        {
                            return ddx2D_<goFloat, GO_FLOAT, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return ddx2D_<goFloat, GO_FLOAT, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::ddx2D(): Type combination ";
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
                            return ddx2D_<goDouble, GO_DOUBLE, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return ddx2D_<goDouble, GO_DOUBLE, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::ddx2D(): Type combination ";
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
                msg = "goMath::ddx2D(): Input type";
                msg += "'"; msg += sig.getDataType().getString().toCharPtr();
                msg += "' not supported."; 
                goLog::warning(msg);
                return false;
            }
            break;
    }
    return false;    
}

/**
* @brief  Calculates the derivative in y direction assuming a 2D signal.
*
* This function calculates the derivative in y-direction while assuming
* the signal is 2-dimensional.
* The arguments are restricted to signals of type GO_FLOAT and
* GO_DOUBLE. 
*
* @todo Implement for 3D-signals.
* 
* @param sig       Signal to calculate the derivative of.
* @param retValue  After the function returned true, contains
*                  the y-derivative of sig. Must be of the same
*                  size as sig.
*
* @return 
**/
bool goMath::ddy2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue)
{
    switch (sig.getDataType().getID())
    {
        case GO_FLOAT:
            {
                switch (retValue.getDataType().getID())
                {
                    case GO_FLOAT:
                        {
                            return ddy2D_<goFloat, GO_FLOAT, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return ddy2D_<goFloat, GO_FLOAT, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::ddy2D(): Type combination ";
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
                            return ddy2D_<goDouble, GO_DOUBLE, goFloat, GO_FLOAT> (sig, retValue);
                        }
                        break;
                    case GO_DOUBLE:
                        {
                            return ddy2D_<goDouble, GO_DOUBLE, goDouble, GO_DOUBLE> (sig, retValue);
                        }
                        break;
                    default:
                        {
                            goString msg;
                            msg = "goMath::ddy2D(): Type combination ";
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
                msg = "goMath::ddy2D(): Input type";
                msg += "'"; msg += sig.getDataType().getString().toCharPtr();
                msg += "' not supported."; 
                goLog::warning(msg);
                return false;
            }
            break;
    }
    return false;    
}

template <class Tx, class Tret>
static bool centralDifferences2_ (const goSignal3DBase<void>& x, goSignal3D<void>& retValue, int dimension, goDouble h)
{
    if (retValue.getSizeX() != x.getSizeX() ||
        retValue.getSizeY() != x.getSizeY() ||    
        retValue.getSizeZ() != x.getSizeZ())
    {
        retValue.make (x.getSizeX(), x.getSizeY(), x.getSizeZ(),
                       x.getBlockSizeX(), x.getBlockSizeY(), x.getBlockSizeZ(),
                       1, 1, 1);
    }
    goSignal3DGenericConstIterator itX (&x);
    goSignal3DGenericIterator itResult (&retValue);

    goDouble h2 = 1.0 / (2*h);
    
    switch (dimension)
    {
        case 0:
            {
                while (!itX.endZ())
                {
                    itX.resetY();
                    itResult.resetY();
                    while (!itX.endY())
                    {
                        itX.resetX();
                        itResult.resetX();
                        while (!itX.endX())
                        {
                            *(Tret*)*itResult = (*(const Tx*)itX.rightX() - *(const Tx*)itX.leftX()) * h2;
                            itX.incrementX();
                            itResult.incrementX();
                        }
                        itX.incrementY();
                        itResult.incrementY();
                    }
                    itX.incrementZ();
                    itResult.incrementZ();
                }
            }
            break;
        case 1:
            {
                while (!itX.endZ())
                {
                    itX.resetY();
                    itResult.resetY();
                    while (!itX.endY())
                    {
                        itX.resetX();
                        itResult.resetX();
                        while (!itX.endX())
                        {
                            *(Tret*)*itResult = (*(const Tx*)itX.rightY() - *(const Tx*)itX.leftY()) * h2;
                            itX.incrementX();
                            itResult.incrementX();
                        }
                        itX.incrementY();
                        itResult.incrementY();
                    }
                    itX.incrementZ();
                    itResult.incrementZ();
                }
            }
            break;
        case 2:
            {
                while (!itX.endZ())
                {
                    itX.resetY();
                    itResult.resetY();
                    while (!itX.endY())
                    {
                        itX.resetX();
                        itResult.resetX();
                        while (!itX.endX())
                        {
                            *(Tret*)*itResult = (*(const Tx*)itX.rightZ() - *(const Tx*)itX.leftZ()) * h2;
                            itX.incrementX();
                            itResult.incrementX();
                        }
                        itX.incrementY();
                        itResult.incrementY();
                    }
                    itX.incrementZ();
                    itResult.incrementZ();
                }
            }
            break;
        default:
            {
                goLog::error ("goMath::centralDifferences(): Only dimensions 0,1,2 are valid.");
                return false;
            }
            break;
    }
    return true;
}

template <class T>
static bool centralDifferences_ (const goSignal3DBase<void>& x, goSignal3D<void>& retValue, int dimension, goDouble h)
{
    switch (retValue.getDataType().getID())
    {
        case GO_FLOAT: return centralDifferences2_<T, goFloat> (x,retValue,dimension, h); break; 
        case GO_DOUBLE: return centralDifferences2_<T, goDouble> (x,retValue,dimension, h); break; 
        default:
            {
                goLog::error ("centralDifferencesX(): Unsupported data type for retValue.");
                return false;
            }
    }
}

/** 
 * @brief  Calculate central finite differences in a given direction.
 * 
 * @param x          Data grid.
 * @param retValue   Contains finite differences after the function returns true.
 *                   If the size of retValue does not match the size of
 *                   x, retValue
 *                   will be resized to the size of x, 
 *                   blocksize of x and border of 1 in each direction.
 * @param dimension  Dimension (0, 1, or 2 for x, y, or z)
 * @param h          Grid spacing (default 1)
 * 
 * @note Only goFloat and goDouble data are supported. The data types of x and retValue may differ.
 *       Both are given by the user, so the data type of retValue must be set before calling 
 *       this function.
 *
 * @return  True if successful, false otherwise.
 * @author Christian Gosch
 */
bool goMath::centralDifferences (const goSignal3DBase<void>& x, goSignal3D<void>& retValue, int dimension, goDouble h)
{
    switch (x.getDataType().getID())
    {
        case GO_FLOAT: return centralDifferences_<goFloat> (x,retValue,dimension,h); break;
        case GO_DOUBLE: return centralDifferences_<goDouble> (x,retValue,dimension,h); break;
        default:
            {
                goLog::error ("centralDifferences(): Unsupported data type for x.");
                return false;
            }
    }
}
/** @} */
