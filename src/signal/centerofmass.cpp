/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal3d.h>
#include <gosignalhelper.h>
#include <gosignal3dgenericiterator.h>

template <class T>
static inline bool _signalCOM2 (const goSignal3DBase<void>& sig, goMath::Vectord& comRet)
{
    if (comRet.getSize() != 3)
    {
        comRet.resize (3);
    }
    comRet.fill (0.0);
    goSignal3DGenericConstIterator it(&sig);
    goDouble xyz_[] = {0.0, 0.0, 0.0};
    goMath::Vectord xyz (xyz_, 3, 1);
    goDouble total = 0.0;
    goDouble temp = 0.0;
    while (!it.endZ())
    {
        it.resetY();
        xyz[1] = 0.0;
        while (!it.endY())
        {
            it.resetX();
            xyz[0] = 0.0;
            while (!it.endX())
            {
                temp = *(T*)*it;
                comRet += xyz * temp;
                total += temp;
                xyz[0] += 1.0;
                it.incrementX();
            }
            xyz[1] += 1.0;
            it.incrementY();
        }
        xyz[2] += 1.0;
        it.incrementZ();
    }
    comRet /= total;
    return true;
}

/** 
 * @brief Calculate centre of mass of \c sig.
 * 
 * @param sig    Data.
 * @param comRet On returning true, contains the centre of mass.
 * 
 * @return True if successful, false otherwise.
 */
bool goSignalCOM (const goSignal3DBase<void>& sig, goMath::Vectord& comRet)
{
    switch (sig.getDataType().getID())
    {
        case GO_UINT8: return _signalCOM2<goUInt8> (sig, comRet); break;
        case GO_INT8: return _signalCOM2<goInt8> (sig, comRet); break;
        case GO_UINT16: return _signalCOM2<goUInt16> (sig, comRet); break;
        case GO_INT16: return _signalCOM2<goInt16> (sig, comRet); break;
        case GO_UINT32: return _signalCOM2<goUInt32> (sig, comRet); break;
        case GO_INT32: return _signalCOM2<goInt32> (sig, comRet); break;
        case GO_FLOAT: return _signalCOM2<goFloat> (sig, comRet); break;
        case GO_DOUBLE: return _signalCOM2<goDouble> (sig, comRet); break;
        default: goLog::error ("goSignalCOM(): unsupported data type."); break;
    }
    return false;
}

