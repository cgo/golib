#ifndef GOMATH_H
# include <gomath.h>
#endif
#include <gosignal3dbase.h>
#include <golog.h>
#include <gostring.h>

bool goMath::laplacian2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue)
{
    if (sig.getDataType().getID() != GO_FLOAT || retValue.getDataType().getID() != GO_FLOAT)
    {
        goString msg = "goMath::laplacian2D: signal and return value should be float-type, instead they are ";
        msg += sig.getDataType().getString().toCharPtr(); msg += " and "; msg += retValue.getDataType().getString().toCharPtr();
        goLog::warning (msg);
        return false;
    }
    if (retValue.getSizeX() != sig.getSizeX() ||
        retValue.getSizeY() != sig.getSizeY() ||
        retValue.getSizeZ() != 1)
    {
        goString msg = "goMath::laplacian2D: return signal size not correct.";
        msg += "Size is ";
        msg += (int)retValue.getSizeX(); msg += " "; msg += (int)retValue.getSizeY(); msg += " "; msg += (int)retValue.getSizeZ();
        msg += " but should be "; (int)sig.getSizeX(); msg += " "; msg += (int)sig.getSizeY(); msg += " 1";
        goLog::warning (msg);
        return false;
    }
    
    const goPtrdiff_t* sigDx = NULL;
    const goPtrdiff_t* sigDy = sig.getYDiff();
    goPtrdiff_t* retDx = NULL;
    goPtrdiff_t* retDy = retValue.getYDiff();
    const goByte* sigP;
    goByte*       retP;

    goIndex_t i,j;
    goFloat u[9];
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
            *(goFloat*)retP = lambda * (*(goFloat*)(sigP + *sigDx) + *(goFloat*)(sigP - *(sigDx-1)) + *(goFloat*)(sigP + *sigDy) + *(goFloat*)(sigP - *(sigDy-1)) - 4**(goFloat*)sigP) + 
            _lambda * (*(goFloat*)(sigP + *sigDx + *sigDy) + *(goFloat*)(sigP - *(sigDx-1) + *sigDy) + *(goFloat*)(sigP + *sigDx - *(sigDy-1)) + *(goFloat*)(sigP - *(sigDx-1) - *(sigDy-1)) - 4**(goFloat*)sigP) * 0.5f;

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

bool goMath::gradient2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue)
{
    if (sig.getDataType().getID() != GO_FLOAT || retValue.getDataType().getID() != GO_FLOAT)
    {
        goString msg = "goMath::gradient2D: signal and return value should be float-type, instead they are ";
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

    goIndex_t i,j;
    goFloat u[9];
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
            *(goFloat*)retP1 = lambda * (*(goFloat*)(sigP + *sigDx) - *(goFloat*)(sigP - *(sigDx-1))) + 
                               _lambda * (*(goFloat*)(sigP + *sigDx + *sigDy) - *(goFloat*)(sigP - *(sigDx-1) + *sigDy) + *(goFloat*)(sigP + *sigDx - *(sigDy-1)) - *(goFloat*)(sigP - *(sigDx-1) - *(sigDy-1))) * 0.5f;
            *(goFloat*)retP2 = lambda * (*(goFloat*)(sigP + *sigDy) - *(goFloat*)(sigP - *(sigDy-1))) + 
                               _lambda * (*(goFloat*)(sigP + *sigDx + *sigDy) - *(goFloat*)(sigP - *(sigDy-1) + *sigDx) + *(goFloat*)(sigP + *sigDy - *(sigDx-1)) - *(goFloat*)(sigP - *(sigDx-1) - *(sigDy-1))) * 0.5f;

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
