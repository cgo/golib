/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gosignalhelper.cpp,v 1.2 2006/04/21 18:38:32 gosch Exp $
 */

#include <gosignalhelper.h>
#include <golog.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosignalmacros.h>
#include <gosignal3dgenericiterator.h>
#include <gotype.h>
#include <gomath.h>
#ifndef GOTYPE_HPP
# include <gotype.hpp>
#endif

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
/*!
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
 *
 * @return True if successful, false otherwise (e.g. if the signal is not float or double).
 **/
bool goNormalizeSignal (goSignal3DBase<void>* sig)
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
                goNormalizeSignal__ ((goFloat)minimum, (goFloat)maximum, sig, sig);
            }
            break;
        case GO_DOUBLE:
            {
                goNormalizeSignal__ ((goDouble)minimum, (goDouble)maximum, sig, sig);
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

template <class T>
static bool convertSignal (const goSignal3DBase<void>* sig, 
                           goSignal3DBase<void>* targetSig,
                           bool useGivenMinMaxValues = false, 
                           T minValue = T(0), 
                           T maxValue = T(0),
                           bool useGivenMinMaxIndex = false, 
                           goIndex_t minIndex = 0, 
                           goIndex_t maxIndex = 0);

/*
 * Signals must be of same size.
 * givenMinValue/maxValue can optionally determine the min and max values of targetSig
 * and givenMinIndex/maxIndex the min/max indices of the source signal sig.
 */
template <class T>
static bool convertSignal (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig,
                           bool useGivenMinMaxValues, T givenMinValue, T givenMaxValue,
                           bool useGivenMinMaxIndex, goIndex_t givenMinIndex, goIndex_t givenMaxIndex)
{
    goIndex_t minIndex = sig->getDataType().getMinIndex();
    goIndex_t maxIndex = sig->getDataType().getMaxIndex();
    if (useGivenMinMaxIndex == true)
    {
        minIndex = givenMinIndex;
        maxIndex = givenMaxIndex;
    }
    goArray<T> lut;
    T* lutP = 0;
    goIndexFunction indexFunction = sig->getDataType().getIndexFunction();
    if (useGivenMinMaxValues == true)
    {
        lutP = goCreateQuantizationTable (sig->getDataType(), givenMinValue, givenMaxValue, 
                                          minIndex, 
                                          maxIndex, lut);
    }
    else
    {
        T minValue;
        T maxValue;
        switch (targetSig->getDataType().getID())
        {
            case GO_FLOAT:
            case GO_DOUBLE:
                {
                    minValue = static_cast<T>(0.0f);
                    maxValue = static_cast<T>(1.0f);
                }
                break;
            default: 
                {
                    minValue = static_cast<T>(targetSig->getDataType().getMinimum());
                    maxValue = static_cast<T>(targetSig->getDataType().getMaximum());
                }
                break;
        }
        lutP = goCreateQuantizationTable (sig->getDataType(), minValue, maxValue, minIndex, maxIndex, lut);
    }
    goSize_t channelCount = goMath::min(sig->getChannelCount(), targetSig->getChannelCount());
    goSize_t i;
    goSignal3DGenericIterator targetIt (targetSig);
    goSignal3DGenericConstIterator sourceIt (sig);
    for (i = 0; i < channelCount; ++i)
    {
        (const_cast<goSignal3DBase<void>*>(sig))->setChannel(i);
        targetSig->setChannel(i);
        sourceIt.resetZ();
        targetIt.resetZ();
        while (!targetIt.endZ())
        {
            targetIt.resetY();
            sourceIt.resetY();
            while (!targetIt.endY())
            {
                targetIt.resetX();
                sourceIt.resetX();
                while (!targetIt.endX())
                {
                    *(T*)*targetIt = lutP[indexFunction(*sourceIt)];
                    targetIt.incrementX();
                    sourceIt.incrementX();
                }
                targetIt.incrementY();
                sourceIt.incrementY();
            }
            targetIt.incrementZ();
            targetIt.incrementZ();
        }
    }
    (const_cast<goSignal3DBase<void>*>(sig))->setChannel(0);
    targetSig->setChannel(0);
    return true;
}

/** --------------------------------------------------------------------------
 * @brief Convert a signal to another signal with a given data type.
 * 
 * The signal sig will be converted if possible to targetSig. The target data type
 * is determined by the data type of targetSig.
 * (set with targetSig->setDataType()).
 * The size of the target signal must be the same as the source signal.
 * The signals should only differ in their data types.
 * 
 * @param sig        Pointer to the signal to convert.
 * @param targetSig  Pointer to the target signal. The size must be the same as sig.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goConvertSignal (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    if (!targetSig)
    {
        goLog::warning("goConvertSignal(): targetSig == 0");
        return false;                                           
    }
    if (!sig)
    {
        goLog::warning("goConvertSignal(): sig == 0");
        return false;                                           
    }
    if (sig->getChannelCount() != targetSig->getChannelCount())
    {
        goLog::warning("goConvertSignal(): Channel count of source and target signals differs. Converting only up to min(source,target) number of channels.");
    }
    if (sig->getSizeX() != targetSig->getSizeX() ||
        sig->getSizeY() != targetSig->getSizeY() ||
        sig->getSizeZ() != targetSig->getSizeZ())
    {
        goLog::warning("goConvertSignal(): source and target signal's sizes differ. Not converting.");
        return false;
    }
    switch (targetSig->getDataType().getID())
    {
        case GO_INT8:   return convertSignal<goInt8>   (sig, targetSig); break;
        case GO_UINT8:  return convertSignal<goUInt8>  (sig, targetSig); break;
        case GO_INT16:  return convertSignal<goInt16>  (sig, targetSig); break;
        case GO_UINT16: return convertSignal<goUInt16> (sig, targetSig); break;
        case GO_INT32:  return convertSignal<goInt32>  (sig, targetSig); break;
        case GO_UINT32: return convertSignal<goUInt32> (sig, targetSig); break;
        case GO_FLOAT:  return convertSignal<goFloat>  (sig, targetSig); break;
        case GO_DOUBLE: return convertSignal<goDouble> (sig, targetSig); break;
        default: goLog::warning("goConvertSignal(): unknown data type."); 
                 return false;
                 break;
    }
    return false;
}

template <class T, class targetT>
static bool copySignalChannel2 (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    goSignal3DGenericIterator targetIt (targetSig);
    goSignal3DGenericConstIterator it (sig);

    while (!it.endZ() && !targetIt.endZ())
    {
        it.resetY();
        targetIt.resetY();
        while (!it.endY() && !targetIt.endY())
        {
            it.resetX();
            targetIt.resetX();
            while (!it.endX() && !targetIt.endX())
            {
                *(targetT*)*targetIt = static_cast<targetT>(*(T*)*it);
                it.incrementX();
                targetIt.incrementX();
            }
            it.incrementY();
            targetIt.incrementY();
        }
        it.incrementZ();
        targetIt.incrementZ();
    }
    return true;
}

template <class T>
static bool copySignalChannel (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    switch (targetSig->getDataType().getID())
    {
        case   GO_INT8:     return copySignalChannel2<T,goInt8>     (sig,   targetSig);   break;
        case   GO_UINT8:    return copySignalChannel2<T,goUInt8>    (sig,   targetSig);   break;
        case   GO_INT16:    return copySignalChannel2<T,goInt16>    (sig,   targetSig);   break;
        case   GO_UINT16:   return copySignalChannel2<T,goUInt16>   (sig,   targetSig);   break;
        case   GO_INT32:    return copySignalChannel2<T,goInt32>    (sig,   targetSig);   break;
        case   GO_UINT32:   return copySignalChannel2<T,goUInt32>   (sig,   targetSig);   break;
        case   GO_FLOAT:    return copySignalChannel2<T,goFloat>    (sig,   targetSig);   break;
        case   GO_DOUBLE:   return copySignalChannel2<T,goDouble>   (sig,   targetSig);   break;
        default: return false;
    }
}

/** --------------------------------------------------------------------------
 * @brief Copies a channel from a signal to another signal.
 * 
 * The signals do not have to have the same data type.
 * The size is not checked, but each dimension is only copied until
 * the smallest size is reached.
 * The currently active channel from sig is copied to the currently 
 * active channel of targetSig.
 * 
 * @param sig       Signal to copy.
 * @param targetSig Signal to hold the target.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goCopySignalChannel (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    if (!targetSig)
    {
        goLog::warning("goCopySignal(): targetSig == 0");
        return false;                                           
    }
    if (!sig)
    {
        goLog::warning("goCopySignal(): sig == 0");
        return false;                                           
    }

//    goString msg = "goCopySignal(): Copying ";
//    msg += sig->getObjectName().toCharPtr();
//    msg += " ("; msg += sig->getDataType().getString().toCharPtr();
//    msg += ") to "; msg += targetSig->getObjectName().toCharPtr();
//    msg += " ("; msg += targetSig->getDataType().getString().toCharPtr(); msg += ")";
//    goLog::message (msg.toCharPtr());
    
    switch (sig->getDataType().getID())
    {
        case   GO_INT8:     return copySignalChannel<goInt8>     (sig,   targetSig);   break;
        case   GO_UINT8:    return copySignalChannel<goUInt8>    (sig,   targetSig);   break;
        case   GO_INT16:    return copySignalChannel<goInt16>    (sig,   targetSig);   break;
        case   GO_UINT16:   return copySignalChannel<goUInt16>   (sig,   targetSig);   break;
        case   GO_INT32:    return copySignalChannel<goInt32>    (sig,   targetSig);   break;
        case   GO_UINT32:   return copySignalChannel<goUInt32>   (sig,   targetSig);   break;
        case   GO_FLOAT:    return copySignalChannel<goFloat>    (sig,   targetSig);   break;
        case   GO_DOUBLE:   return copySignalChannel<goDouble>   (sig,   targetSig);   break;
        default: goLog::warning("goCopySignal(): unknown type."); break;
    }
    return false;
}

template <class T, class targetT>
static bool copySignal2 (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    goSignal3DGenericIterator targetIt (targetSig);
    goSignal3DGenericConstIterator it (sig);

    goIndex_t chanCount = goMath::min<goIndex_t>(sig->getChannelCount(),targetSig->getChannelCount());
    goIndex_t j = 0;
    while (!it.endZ() && !targetIt.endZ())
    {
        it.resetY();
        targetIt.resetY();
        while (!it.endY() && !targetIt.endY())
        {
            it.resetX();
            targetIt.resetX();
            while (!it.endX() && !targetIt.endX())
            {
                for (j = 0; j < chanCount; ++j)
                {
                    *(((targetT*)*targetIt) + j) = static_cast<targetT>(*(((const T*)*it) + j));
                }
                it.incrementX();
                targetIt.incrementX();
            }
            it.incrementY();
            targetIt.incrementY();
        }
        it.incrementZ();
        targetIt.incrementZ();
    }
    return true;
}

template <class T>
static bool copySignal (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    switch (targetSig->getDataType().getID())
    {
        case   GO_INT8:     return copySignal2<T,goInt8>     (sig,   targetSig);   break;
        case   GO_UINT8:    return copySignal2<T,goUInt8>    (sig,   targetSig);   break;
        case   GO_INT16:    return copySignal2<T,goInt16>    (sig,   targetSig);   break;
        case   GO_UINT16:   return copySignal2<T,goUInt16>   (sig,   targetSig);   break;
        case   GO_INT32:    return copySignal2<T,goInt32>    (sig,   targetSig);   break;
        case   GO_UINT32:   return copySignal2<T,goUInt32>   (sig,   targetSig);   break;
        case   GO_FLOAT:    return copySignal2<T,goFloat>    (sig,   targetSig);   break;
        case   GO_DOUBLE:   return copySignal2<T,goDouble>   (sig,   targetSig);   break;
        default: return false;
    }
}

/** --------------------------------------------------------------------------
 * @brief Copies a signal to another signal.
 * 
 * The signals do not have to have the same data type.
 * The size is not checked, but each dimension is only copied until
 * the smallest size is reached.
 * Same holds for channels. All channels are copied until one of the signals runs out of channels.
 * 
 * @param sig       Signal to copy.
 * @param targetSig Signal to hold the target.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goCopySignal (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    if (!targetSig)
    {
        goLog::warning("goCopySignal(): targetSig == 0");
        return false;                                           
    }
    if (!sig)
    {
        goLog::warning("goCopySignal(): sig == 0");
        return false;                                           
    }

//    goString msg = "goCopySignal(): Copying ";
//    msg += sig->getObjectName().toCharPtr();
//    msg += " ("; msg += sig->getDataType().getString().toCharPtr();
//    msg += ") to "; msg += targetSig->getObjectName().toCharPtr();
//    msg += " ("; msg += targetSig->getDataType().getString().toCharPtr(); msg += ")";
//    goLog::message (msg.toCharPtr());
    
    switch (sig->getDataType().getID())
    {
        case   GO_INT8:     return copySignal<goInt8>     (sig,   targetSig);   break;
        case   GO_UINT8:    return copySignal<goUInt8>    (sig,   targetSig);   break;
        case   GO_INT16:    return copySignal<goInt16>    (sig,   targetSig);   break;
        case   GO_UINT16:   return copySignal<goUInt16>   (sig,   targetSig);   break;
        case   GO_INT32:    return copySignal<goInt32>    (sig,   targetSig);   break;
        case   GO_UINT32:   return copySignal<goUInt32>   (sig,   targetSig);   break;
        case   GO_FLOAT:    return copySignal<goFloat>    (sig,   targetSig);   break;
        case   GO_DOUBLE:   return copySignal<goDouble>   (sig,   targetSig);   break;
        default: goLog::warning("goCopySignal(): unknown type."); break;
    }
    return false;
}

template <class T, class targetT>
static bool copySignalLinear (const goSignal3DBase<void>* sig, targetT* targetArray)
{
    goSignal3DGenericConstIterator it (sig);

    while (!it.endZ())
    {
        it.resetY();
        while (!it.endY())
        {
            it.resetX();
            while (!it.endX())
            {
                *targetArray = static_cast<targetT>(*(T*)*it);
                it.incrementX();
                ++targetArray;
            }
            it.incrementY();
        }
        it.incrementZ();
    }
    return true;
}

template <class T>
bool goCopySignalArray (const goSignal3DBase<void>* sig, T* targetArray)
{
    if (!targetArray)
    {
        goLog::warning("goCopySignalArray(): targetArray == 0");
        return false;                                           
    }
    if (!sig)
    {
        goLog::warning("goCopySignalArray(): sig == 0");
        return false;                                           
    }

//    goString msg = "goCopySignalArray(): Copying to linear array: ";
//    msg += sig->getObjectName().toCharPtr();
//    msg += " ("; msg += sig->getDataType().getString().toCharPtr();
//    msg += ") to linear array."; 
//    goLog::message (msg.toCharPtr());
    
    switch (sig->getDataType().getID())
    {
        case   GO_INT8:     return copySignalLinear<goInt8,T>     (sig,   targetArray);   break;
        case   GO_UINT8:    return copySignalLinear<goUInt8,T>    (sig,   targetArray);   break;
        case   GO_INT16:    return copySignalLinear<goInt16,T>    (sig,   targetArray);   break;
        case   GO_UINT16:   return copySignalLinear<goUInt16,T>   (sig,   targetArray);   break;
        case   GO_INT32:    return copySignalLinear<goInt32,T>    (sig,   targetArray);   break;
        case   GO_UINT32:   return copySignalLinear<goUInt32,T>   (sig,   targetArray);   break;
        case   GO_FLOAT:    return copySignalLinear<goFloat,T>    (sig,   targetArray);   break;
        case   GO_DOUBLE:   return copySignalLinear<goDouble,T>   (sig,   targetArray);   break;
        default: goLog::warning("goCopySignalArray(): unknown type."); break;
    }
    return false;
}

template <class T, class targetT>
static bool copySignalLinear (const T* array, goSignal3DBase<void>* targetSig)
{
    goSignal3DGenericIterator it (targetSig);

    while (!it.endZ())
    {
        it.resetY();
        while (!it.endY())
        {
            it.resetX();
            while (!it.endX())
            {
                *(targetT*)*it = static_cast<targetT>(*array);
                it.incrementX();
                ++array;
            }
            it.incrementY();
        }
        it.incrementZ();
    }
    return true;
}

template <class T>
bool goCopySignalArray (const T* array, goSignal3DBase<void>* targetSig)
{
    if (!targetSig)
    {
        goLog::warning("goCopySignalArray(): targetSig == 0");
        return false;                                           
    }
    if (!array)
    {
        goLog::warning("goCopySignalArray(): array == 0");
        return false;                                           
    }

//    goString msg = "goCopySignalArray(): Copying from linear array: ";
//    msg += targetSig->getObjectName().toCharPtr();
//    msg += " ("; msg += targetSig->getDataType().getString().toCharPtr();
//    msg += ") from linear array."; 
//    goLog::message (msg.toCharPtr());
    
    switch (targetSig->getDataType().getID())
    {
        case   GO_INT8:     return copySignalLinear<T,goInt8>   (array, targetSig);   break;
        case   GO_UINT8:    return copySignalLinear<T,goUInt8>  (array, targetSig);   break;
        case   GO_INT16:    return copySignalLinear<T,goInt16>  (array, targetSig);   break;
        case   GO_UINT16:   return copySignalLinear<T,goUInt16> (array, targetSig);   break;
        case   GO_INT32:    return copySignalLinear<T,goInt32>  (array, targetSig);   break;
        case   GO_UINT32:   return copySignalLinear<T,goUInt32> (array, targetSig);   break;
        case   GO_FLOAT:    return copySignalLinear<T,goFloat>  (array, targetSig);   break;
        case   GO_DOUBLE:   return copySignalLinear<T,goDouble> (array, targetSig);   break;
        default: goLog::warning("goCopySignalArray(): unknown type."); break;
    }
    return false;
}

template <class sourceT, class targetT>
static bool _RGBAtoScalar (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    goSignal3DGenericConstIterator sourceIt (sig);
    goSignal3DGenericIterator targetIt (targetSig);
    // goPtrdiff_t redOffset = 0 * sizeof(sourceT);
    goPtrdiff_t greenOffset = 1 * sizeof(sourceT);
    goPtrdiff_t blueOffset = 2 * sizeof(sourceT);
    goDouble third = 1.0 / 3.0;
    while (!sourceIt.endZ())
    {
        sourceIt.resetY();
        targetIt.resetY();
        while (!sourceIt.endY())
        {
            sourceIt.resetX();
            targetIt.resetX();
            while (!sourceIt.endX())
            {
                *(targetT*)*targetIt = static_cast<targetT>((*(sourceT*)*sourceIt + 
                                        *(sourceT*)(*sourceIt + greenOffset) +
                                        *(sourceT*)(*sourceIt + blueOffset)) * third);
                sourceIt.incrementX();
                targetIt.incrementX();
            }
            sourceIt.incrementY();
            targetIt.incrementY();
        }
        sourceIt.incrementZ();
        targetIt.incrementZ();
    }
    return true;
}

/** --------------------------------------------------------------------------
 * @brief Converts a RGBA 4-channel signal to a scalar 1-channel signal.
 * 
 * The source signal must have 3 or 4 channels that are interpreted as
 * RGB(A).
 * 
 * @param sig       Source signal, must be of type GO_UINT8 and have >= 3 channels.
 * @param targetSig Target signal. Data type must be set and the size must be the same as
 *                  that of the source signal.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goRGBAtoScalar (const goSignal3DBase<void>* sig, goSignal3DBase<void>* targetSig)
{
    if (!sig || !targetSig)
    {
        return false;
    }
    if (sig->getChannelCount() < 3)
    {
        goLog::warning("goRGBAtoScalar(): source signal has less than 3 channels. Not converting.");
        return false;
    }
    if (sig->getDataType().getID() != GO_UINT8)
    {
        goLog::warning("goRGBAtoScalar(): currently only supporting 8 bit RGBA conversion.");
        return false;
    }
    if (sig->getSizeX() != targetSig->getSizeX() ||
        sig->getSizeY() != targetSig->getSizeY() ||
        sig->getSizeZ() != targetSig->getSizeZ())
    {
        goLog::warning("goRGBAtoScalar(): source and target signal sizes mismatch. Not converting.");
        return false;
    }

    const_cast<goSignal3DBase<void>*>(sig)->setChannel (0);
    goSignal3DGenericConstIterator sourceIt (sig);
    goSignal3DGenericIterator targetIt (targetSig);
    
    switch (targetSig->getDataType().getID())
    {
        case GO_UINT8:  return _RGBAtoScalar<goUInt8,goUInt8>  (sig, targetSig); break;
        case GO_INT8:   return _RGBAtoScalar<goUInt8,goInt8>   (sig, targetSig); break;
        case GO_UINT16: return _RGBAtoScalar<goUInt8,goUInt16> (sig, targetSig); break;
        case GO_INT16:  return _RGBAtoScalar<goUInt8,goInt16>  (sig, targetSig); break;
        case GO_UINT32: return _RGBAtoScalar<goUInt8,goUInt32> (sig, targetSig); break;
        case GO_INT32:  return _RGBAtoScalar<goUInt8,goInt32>  (sig, targetSig); break;
        case GO_FLOAT:  
                  {
                      bool ok = _RGBAtoScalar<goUInt8,goFloat>  (sig, targetSig); 
                      if (ok)
                      {
                          goNormalizeSignal(targetSig);
                      }
                      return ok;
                  }
                  break;
        case GO_DOUBLE: 
                  {
                      bool ok = _RGBAtoScalar<goUInt8,goDouble> (sig, targetSig); 
                      if (ok)
                      {
                          goNormalizeSignal(targetSig);
                      }
                      return ok;
                  }
                  break;
        default: goLog::warning("goRGBAtoScalar(): unknown data type."); break;
    }
    return false;
}

template <class T>
static bool _fillSignal (goSignal3DBase<void>* sig, goFloat value)
{
    T v = T(value);
    sig->fill (&v);
    return true;
}

/** --------------------------------------------------------------------------
 * @brief Fills a signal with a float value (will be converted).
 * 
 * @param sig   Signal to fill.
 * @param value Float value. This value will be converted to an appropriate
 *              data type for the signal.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goFillSignal (goSignal3DBase<void>* sig, goFloat value)
{
    if (!sig)
        return false;
    switch (sig->getDataType().getID())
    {
        case GO_INT8: return _fillSignal<goInt8> (sig, value); break;
        case GO_UINT8: return _fillSignal<goUInt8> (sig, value); break;
        case GO_INT16: return _fillSignal<goInt16> (sig, value); break;
        case GO_UINT16: return _fillSignal<goUInt16> (sig, value); break;
        case GO_INT32: return _fillSignal<goInt32> (sig, value); break;
        case GO_UINT32: return _fillSignal<goUInt32> (sig, value); break;
        case GO_FLOAT: return _fillSignal<goFloat> (sig, value); break;
        case GO_DOUBLE: return _fillSignal<goDouble> (sig, value); break;
        default: goLog::warning ("goFillSignal(): unknown type."); return false; break;
    }
    return false;
}

#define MAKE_SIGNAL_SIGNAL_OPERATOR1(OPERATOR,OPERATORNAME) \
template <class T, class T2> \
static inline void _signalOperator##OPERATORNAME__ (goSignal3DBase<void>& sig, const goSignal3DBase<void>& other) \
{ \
    goSignal3DGenericIterator      it (&sig); \
    goSignal3DGenericConstIterator ot (&other); \
     \
    while (!it.endZ() && !ot.endZ()) \
    { \
        it.resetY(); \
        ot.resetY(); \
        while (!it.endY() && !ot.endY()) \
        { \
            it.resetX(); \
            ot.resetX(); \
            while (!it.endX() && !ot.endX()) \
            { \
                *(T*)*it OPERATOR (T)*(T2*)*ot; \
                it.incrementX(); \
                ot.incrementX(); \
            } \
            it.incrementY(); \
            ot.incrementY(); \
        } \
        it.incrementZ(); \
        ot.incrementZ(); \
    } \
} 

#define MAKE_SIGNAL_SIGNAL_OPERATOR2(OPERATOR,OPERATORNAME) \
template <class T>  \
static inline void _signalOperator##OPERATORNAME_ (goSignal3DBase<void>& sig, const goSignal3DBase<void>& other) \
{ \
    switch (other.getDataType().getID()) \
    { \
        case   GO_INT8:     _signalOperator##OPERATORNAME__<T,goInt8>     (sig,   other);   break; \
        case   GO_UINT8:    _signalOperator##OPERATORNAME__<T,goUInt8>    (sig,   other);   break; \
        case   GO_INT16:    _signalOperator##OPERATORNAME__<T,goInt16>    (sig,   other);   break; \
        case   GO_UINT16:   _signalOperator##OPERATORNAME__<T,goUInt16>   (sig,   other);   break; \
        case   GO_INT32:    _signalOperator##OPERATORNAME__<T,goInt32>    (sig,   other);   break; \
        case   GO_UINT32:   _signalOperator##OPERATORNAME__<T,goUInt32>   (sig,   other);   break; \
        case   GO_FLOAT:    _signalOperator##OPERATORNAME__<T,goFloat>    (sig,   other);   break; \
        case   GO_DOUBLE:   _signalOperator##OPERATORNAME__<T,goDouble>   (sig,   other);   break; \
        default: goLog::warning("goSignal3DBase<void> operator+=: unknown type."); break; \
    } \
} 

#define MAKE_SIGNAL_SIGNAL_OPERATOR3(OPERATOR,OPERATORNAME)  \
static inline void _signalOperator##OPERATORNAME (goSignal3DBase<void>& sig, const goSignal3DBase<void>& other) \
{ \
    switch (sig.getDataType().getID()) \
    { \
        case   GO_INT8:     _signalOperator##OPERATORNAME_<goInt8>     (sig,other);   break; \
        case   GO_UINT8:    _signalOperator##OPERATORNAME_<goUInt8>    (sig,other);   break; \
        case   GO_INT16:    _signalOperator##OPERATORNAME_<goInt16>    (sig,other);   break; \
        case   GO_UINT16:   _signalOperator##OPERATORNAME_<goUInt16>   (sig,other);   break; \
        case   GO_INT32:    _signalOperator##OPERATORNAME_<goInt32>    (sig,other);   break; \
        case   GO_UINT32:   _signalOperator##OPERATORNAME_<goUInt32>   (sig,other);   break; \
        case   GO_FLOAT:    _signalOperator##OPERATORNAME_<goFloat>    (sig,other);   break; \
        case   GO_DOUBLE:   _signalOperator##OPERATORNAME_<goDouble>   (sig,other);   break; \
        default: goLog::warning("operator #OPERATOR: unknown type."); break; \
    } \
}

MAKE_SIGNAL_SIGNAL_OPERATOR1(+=,PlusEqual);
MAKE_SIGNAL_SIGNAL_OPERATOR2(+=,PlusEqual);
MAKE_SIGNAL_SIGNAL_OPERATOR3(+=,PlusEqual);
// MAKE_SIGNAL_SIGNAL_OPERATOR1(-=,MinusEqual);
// MAKE_SIGNAL_SIGNAL_OPERATOR2(-=,MinusEqual);
// MAKE_SIGNAL_SIGNAL_OPERATOR3(-=,MinusEqual);
// MAKE_SIGNAL_SIGNAL_OPERATOR1(*=,TimesEqual);
// MAKE_SIGNAL_SIGNAL_OPERATOR2(*=,TimesEqual);
// MAKE_SIGNAL_SIGNAL_OPERATOR3(*=,TimesEqual);

/** --------------------------------------------------------------------------
 * @brief Adds two signals.
 * 
 * Adds other to sig.
 * 
 * @param sig   Signal 1. Will also hold the sum of both signals.
 * @param other The signal to add to the first argument, sig.
 ----------------------------------------------------------------------------*/
void goSignalPlus (goSignal3DBase<void>& sig, const goSignal3DBase<void>& other)
{
    _signalOperatorPlusEqual (sig, other);
}

/** --------------------------------------------------------------------------
 * @brief Get some description (as ASCII text) for a signal.
 * 
 * @param sig    Signal to get the description for.
 * @param strRet String to hold the description.
 * @param html   If true, the text will be HTML formatted, if false, the text
 *               will be plain ASCII.
 ----------------------------------------------------------------------------*/
void goSignalInfoText (const goSignal3DBase<void>& sig, goString& strRet, bool html)
{
    if (html)
    {
        strRet  = "<table bgcolor=\"#80FF80\">";
        strRet += "<tr>";
        strRet += "<td><b>Object class </b></td><td>"; strRet += sig.getClassName(); strRet += "</td>";
        strRet += "</tr>";
        strRet += "<tr>";
        strRet += "<td><b>Object name </b></td><td>"; strRet += sig.getObjectName().toCharPtr(); strRet += "</td>";
        strRet += "</tr>";
        strRet += "<tr>";
        strRet += "<td><b>Dimensions </b></td><td>"; strRet += (int)sig.getSizeX(); 
        strRet += ",";              strRet += (int)sig.getSizeY();
        strRet += ",";              strRet += (int)sig.getSizeZ(); strRet += "</td>";
        strRet += "</tr>";
        strRet += "<tr>";
        strRet += "<td><b>Data type </b></td><td>"; strRet += sig.getDataType().getString().toCharPtr(); strRet += "</td>";
        strRet += "</tr>";
        strRet += "<tr>";
        strRet += "<td><b>Channels </b></td><td>"; strRet += (int)sig.getChannelCount(); strRet += "</td>";
        strRet += "</tr>";
        strRet += "<tr>";
        strRet += "<td><b>Approx. size </b></td><td>"; strRet += (int)sig.memoryUsage(); strRet += " bytes</td>";
        strRet += "</tr>";
        strRet += "</table>";
    }
    else
    {
        strRet = "Object class: "; strRet += sig.getClassName(); strRet += "\n";
        strRet += "Object name: "; strRet += sig.getObjectName().toCharPtr(); strRet += "\n";
        strRet += "Dimensions: "; strRet += (int)sig.getSizeX(); 
        strRet += ",";              strRet += (int)sig.getSizeY();
        strRet += ",";              strRet += (int)sig.getSizeZ(); strRet += "\n";
        strRet += "Data type: "; strRet += sig.getDataType().getString().toCharPtr(); strRet += "\n";
        strRet += "Channels: "; strRet += (int)sig.getChannelCount(); strRet += "\n";
        strRet += "Approx. size: "; strRet += (int)sig.memoryUsage(); strRet += " bytes";
    }
}

template <typename T> static goDouble signalMean (const goSignal3DBase<void>& sig)
{
    goDouble ret = 0.0;
    goSignal3DGenericConstIterator it (&sig);
    while (!it.endZ())
    {
        it.resetY();
        while (!it.endY())
        {
            it.resetX();
            while (!it.endX())
            {
                ret += (goDouble)*(const T*)*it;
                it.incrementX();
            }
            it.incrementY();
        }
        it.incrementZ();
    }
    return ret / static_cast<goDouble>(sig.getSizeX() * sig.getSizeY() * sig.getSizeZ());
}

/** --------------------------------------------------------------------------
 * @brief Calculate the signal mean.
 * 
 * @param sig  Signal to calculate the mean of.
 * 
 * @return  Mean value as goDouble. For multichannel signals,
 *          just do setChannel() and then goSignalMean() for each channel.
 ----------------------------------------------------------------------------*/
goDouble goSignalMean (const goSignal3DBase<void>& sig)
{
    switch (sig.getDataType().getID())
    {
        case GO_INT8: return signalMean<goInt8> (sig); break;
        case GO_UINT8: return signalMean<goUInt8> (sig); break;
        case GO_INT16: return signalMean<goInt16> (sig); break;
        case GO_UINT16: return signalMean<goUInt16> (sig); break;
        case GO_INT32: return signalMean<goInt32> (sig); break;
        case GO_UINT32: return signalMean<goUInt32> (sig); break;
        case GO_FLOAT: return signalMean<goFloat> (sig); break;
        case GO_DOUBLE: return signalMean<goDouble> (sig); break;
        default:
            {
                goLog::warning ("goSignalMean(): unknown data type.");
                return 0.0;
            }
            break;
    }
}

template <typename T> static goDouble signalVariance (const goSignal3DBase<void>& sig, goDouble mean)
{
    goDouble ret = 0.0;
    goDouble temp = 0.0;
    goSignal3DGenericConstIterator it (&sig);
    while (!it.endZ())
    {
        it.resetY();
        while (!it.endY())
        {
            it.resetX();
            while (!it.endX())
            {
                temp = (goDouble)*(const T*)*it - mean;
                ret += temp * temp;
                it.incrementX();
            }
            it.incrementY();
        }
        it.incrementZ();
    }
    return ret / static_cast<goDouble>(sig.getSizeX() * sig.getSizeY() * sig.getSizeZ());
}

/** --------------------------------------------------------------------------
 * @brief Calculate mean and variance of a signal.
 * 
 * @param sig       Signal.
 * @param mean      After returning, holds the mean value as double.
 * @param variance  After returning, holds the variance as double.
 ----------------------------------------------------------------------------*/
void goSignalMeanVariance (const goSignal3DBase<void>& sig, 
                           goDouble& mean, 
                           goDouble& variance)
{
    mean = goSignalMean(sig);
    switch (sig.getDataType().getID())
    {
        case GO_INT8: variance = signalVariance<goInt8> (sig,mean); break;
        case GO_UINT8: variance = signalVariance<goUInt8> (sig,mean); break;
        case GO_INT16: variance = signalVariance<goInt16> (sig,mean); break;
        case GO_UINT16: variance = signalVariance<goUInt16> (sig,mean); break;
        case GO_INT32: variance = signalVariance<goInt32> (sig,mean); break;
        case GO_UINT32: variance = signalVariance<goUInt32> (sig,mean); break;
        case GO_FLOAT: variance = signalVariance<goFloat> (sig,mean); break;
        case GO_DOUBLE: variance = signalVariance<goDouble> (sig,mean); break;
        default:
            {
                goLog::warning ("goSignalVariance(): unknown data type.");
            }
            break;
    }
}


template bool goCopySignalArray<goInt8>   (const goInt8*, goSignal3DBase<void>*);
template bool goCopySignalArray<goUInt8>  (const goUInt8*, goSignal3DBase<void>*);
template bool goCopySignalArray<goInt16>  (const goInt16*, goSignal3DBase<void>*);
template bool goCopySignalArray<goUInt16> (const goUInt16*, goSignal3DBase<void>*);
template bool goCopySignalArray<goInt32>  (const goInt32*, goSignal3DBase<void>*);
template bool goCopySignalArray<goUInt32> (const goUInt32*, goSignal3DBase<void>*);
template bool goCopySignalArray<goFloat>  (const goFloat*, goSignal3DBase<void>*);
template bool goCopySignalArray<goDouble> (const goDouble*, goSignal3DBase<void>*);
template bool goCopySignalArray<goInt8>   (const goSignal3DBase<void>*, goInt8*);
template bool goCopySignalArray<goUInt8>  (const goSignal3DBase<void>*, goUInt8*);
template bool goCopySignalArray<goInt16>  (const goSignal3DBase<void>*, goInt16*);
template bool goCopySignalArray<goUInt16> (const goSignal3DBase<void>*, goUInt16*);
template bool goCopySignalArray<goInt32>  (const goSignal3DBase<void>*, goInt32*);
template bool goCopySignalArray<goUInt32> (const goSignal3DBase<void>*, goUInt32*);
template bool goCopySignalArray<goFloat>  (const goSignal3DBase<void>*, goFloat*);
template bool goCopySignalArray<goDouble> (const goSignal3DBase<void>*, goDouble*);
