#ifndef GOMATH_H
# include <gomath.h>
#endif
#include <gosignal3dbase.h>
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
        retValue.getSizeZ() != 2)
    {
        goString msg = "goMath::gradient2D: return signal size not correct.";
        msg += "Size is ";
        msg += (int)retValue.getSizeX(); msg += " "; msg += (int)retValue.getSizeY(); msg += " "; msg += (int)retValue.getSizeZ();
        msg += " but should be "; (int)sig.getSizeX(); msg += " "; msg += (int)sig.getSizeY(); msg += " 2";
        goLog::warning (msg);
        return false;
    }
    
    const goPtrdiff_t* sigDx = NULL;
    const goPtrdiff_t* sigDy = sig.getYDiff();
    goPtrdiff_t*  retDx = NULL;
    goPtrdiff_t*  retDy = retValue.getYDiff();
    const goByte* sigP;
    goByte*       retP1;
    goByte*       retP2;

    goSize_t i,j;
    // goFloat u[9];
    goFloat lambda = sqrt(2.0f) - 1.0f;
    goFloat _lambda = (1.0f - lambda) * 0.5f;
    for (j = 0; j < sig.getSizeY(); ++j)
    {
        sigP  = (const goByte*)sig.getPtr(0,j,0);
        retP1 = (goByte*)retValue.getPtr(0,j,0);
        retP2 = (goByte*)retValue.getPtr(0,j,1);
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
            *(T_RET*)retP2 = lambda * (*(T*)(sigP + *sigDy) - 
                                         *(T*)(sigP - *(sigDy-1))) + 
                               _lambda * (*(T*)(sigP + *sigDx + *sigDy) - 
                                          *(T*)(sigP - *(sigDy-1) + *sigDx) + 
                                          *(T*)(sigP + *sigDy - *(sigDx-1)) - 
                                          *(T*)(sigP - *(sigDx-1) - *(sigDy-1))) * 0.5f;
            retP1 += *retDx;
            retP2 += *retDx;
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
 * @note  The data type of the arguments is currently restricted to GO_FLOAT,
 *        but GO_DOUBLE will also be implemented.
 *        Multichannel data is not yet supported by libGo.
 *
 * @param sig       Contains the 2D signal.
 * @param retValue  After returning true, retValue contains the 
 *                  x and y components of grad(sig).
 *                  retValue must be of the same size as 
 *                  sig in x and y dimensions and its z dimension must
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
* Currently, the arguments are restricted to signals of type GO_FLOAT.
* GO_DOUBLE will be implemented.
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
* Currently, the arguments are restricted to signals of type GO_FLOAT.
* GO_DOUBLE will be implemented.
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
/** @} */
