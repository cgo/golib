/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#include <gosignalhelper.h>
#include <golog.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosignalmacros.h>

template <class T>
static void goNormalizeSignal__ (T minimum, T maximum, 
                          const goSignal3DBase<void>* sig, 
                          goSignal3DBase<void>*       targetSig)
{
    // See if translation is ok:
    if (maximum - minimum <= 1.0f)
    {
        GO_SIGNAL3D_EACHELEMENT_2_GENERIC (*(T*)__ptr = *(const T*)__ptr_target - minimum, (*targetSig), (*sig));
    }
    else
    {
        // Scale
        goDouble scale = 1.0f / (maximum - minimum);
        GO_SIGNAL3D_EACHELEMENT_2_GENERIC (*(T*)__ptr = (*(const T*)__ptr_target - minimum) * scale, (*targetSig), (*sig));
    }
}
/**
 * @brief Normalizes or translates a float or double type signal to the interval
 *        [0,1].
 *
 * If the signal values are out of the interval [0,1],
 * the signal is normalized to it.
 * If (maxValue - minValue) <= 1.0, the signal values are just translated into
 * [0,1].
 * 
 * @param sig        Signal to be normalized.
 * @param targetSig  Target containing the normalized signal after the call.
 *                   Note that targetSig is reallocated during the call.
 *
 * @return True if successful, false otherwise (e.g. if the signal is not float or double).
 **/
bool goNormalizeSignal (const goSignal3DBase<void>* sig, goSignal3D<void>* targetSig)
{
    if (sig->getDataType().getID() != GO_FLOAT && sig->getDataType().getID() != GO_DOUBLE)
    {
        return false;
    }
    goDouble minimum = 0.0;
    goDouble maximum = 1.0;
    minimum = sig->getMinimum();
    maximum = sig->getMaximum();
    {
        goString msg ("goNormalizeSignal(): min = ");
        msg += goFloat(minimum);
        msg += ", max = "; msg += goFloat(maximum);
        goLog::message(msg);
    }
    if (minimum >= 0.0 && maximum <= 1.0)
    {
        return false;
    }
    switch (sig->getDataType().getID())
    {
        case GO_FLOAT:
            {
                targetSig->setDataType (GO_FLOAT);
                if (!targetSig->make (sig->getSizeX(), 
                            sig->getSizeY(),
                            sig->getSizeZ(),
                            sig->getBlockSizeX(),
                            sig->getBlockSizeY(),
                            sig->getBlockSizeZ(),
                            16, 16, 16))
                {
                    goString msg ("goNormalizeSignal(): Could not allocate memory!");
                    goLog::error (msg);
                    return false;
                }
                goNormalizeSignal__ ((goFloat)minimum, (goFloat)maximum, sig, targetSig);
            }
            break;
        case GO_DOUBLE:
            {
                targetSig->setDataType (GO_DOUBLE);
                if (!targetSig->make (sig->getSizeX(), 
                            sig->getSizeY(),
                            sig->getSizeZ(),
                            sig->getBlockSizeX(),
                            sig->getBlockSizeY(),
                            sig->getBlockSizeZ(),
                            16, 16, 16))
                {
                    goString msg ("goNormalizeSignal(): Could not allocate memory!");
                    goLog::error (msg);
                    return false;
                }
                goNormalizeSignal__ ((goDouble)minimum, (goDouble)maximum, sig, targetSig);
            }
            break;
        default:
            break;
    }
    return true;
}

template <class T, goTypeEnum T_ENUM>
bool goFindZeroCrossings__ (const goSignal3DBase<void>* sig, goArray<goPointf>& pointsRet)
{
    goPointf point;
    pointsRet.resize (0);
    if (sig->getSizeZ() > 1)
    {
        goLog::warning ("goFindZeroCrossings(): Only 2D signals supported so far. Calculating only for first z-slice.");
    }
    GO_SIGNAL3D_EACHELEMENT_GENERIC (
            if (*(const T*)__ptr * *(const T*)(__ptr + *__dx) < T(0))
            {
                point.x = (goFloat)__k;
                point.y = (goFloat)__j + 0.5f;
                point.x += *(const T*)(__ptr) / (*(const T*)(__ptr) - *(const T*)(__ptr  + *__dx));
                pointsRet += point;
            }
            if (*(const T*)__ptr * *(const T*)(__ptr + *__dy) < T(0))
            {
                point.x = (goFloat)__k + 0.5f;
                point.y = (goFloat)__j;
                point.y += *(const T*)(__ptr) / (*(const T*)(__ptr) - *(const T*)(__ptr  + *__dy));
                pointsRet += point;
            }, (*sig));
    return true;
}

bool goFindZeroCrossings (const goSignal3DBase<void>* sig, goArray<goPointf>& pointsRet)
{
    if (!sig)
    {
        return false;
    }
    switch (sig->getDataType().getID())
    {
        case GO_FLOAT:
            {
                return goFindZeroCrossings__<goFloat, GO_FLOAT> (sig, pointsRet);
            }
            break;
        case GO_DOUBLE:
            {
                return goFindZeroCrossings__<goDouble, GO_DOUBLE> (sig, pointsRet);
            }
            break;
        default:
            {
                goString msg;
                msg = "goFindZeroCrossings(): Type ";
                msg += sig->getDataType().getString().toCharPtr();
                msg += " not supported. Only float and double.";
                goLog::warning (msg);
                return false;
            }
            break;
    }
    return false;
}
