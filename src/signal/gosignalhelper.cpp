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
