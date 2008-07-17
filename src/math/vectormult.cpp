#ifndef GOMATHSIGNAL_H
# include <gomathsignal.h>
#endif
#ifndef GOSIGNAL3DGENERICITERATOR_H
# include <gosignal3dgenericiterator.h>
#endif
#ifndef GOLOG_H
# include <golog.h>
#endif

template <class T1,class T2,class TR>
static inline bool vectorMult3 (const goSignal3DBase<void>& V1, const goSignal3DBase<void>& V2, goSignal3DBase<void>& result)
{
    //= This is dirty but we have to do it.
    goSize_t v1Channel = V1.getChannel();
    goSize_t v2Channel = V2.getChannel();
    const_cast<goSignal3DBase<void>& >(V1).setChannel(0);
    const_cast<goSignal3DBase<void>& >(V2).setChannel(0);
    
    const goIndex_t channelCount = static_cast<goIndex_t>(V1.getChannelCount());
    
    goSignal3DGenericConstIterator it1 (&V1);
    goSignal3DGenericConstIterator it2 (&V2);
    goSignal3DGenericIterator      itR (&result);
    while (!it1.endZ())
    {
        it1.resetY();
        it2.resetY();
        itR.resetY();
        while (!it1.endY())
        {
            it1.resetX();
            it2.resetX();
            itR.resetX();
            while (!it1.endX())
            {
                T1* p1 = (T1*)*it1;
                T2* p2 = (T2*)*it2;
                goDouble r = 0.0;
                goIndex_t i;
                //= ***** Assume channels are linear in memory *****
                //= we save using getChannelOffset() here.
                for (i = 0; i < channelCount; ++i,++p1,++p2)
                {
                    r += *p1 * *p2;
                }
                *(TR*)*itR = TR(r); 
                it1.incrementX();
                it2.incrementX();
                itR.incrementX();
            }
            it1.incrementY();
            it2.incrementY();
            itR.incrementY();
        }
        it1.incrementZ();
        it2.incrementZ();
        itR.incrementZ();
    }

    //= Restore channel.
    const_cast<goSignal3DBase<void>& >(V1).setChannel(v1Channel);
    const_cast<goSignal3DBase<void>& >(V2).setChannel(v2Channel);
    return true;
}


    template <class T1,class T2>
static inline bool vectorMult2 (const goSignal3DBase<void>& V1, const goSignal3DBase<void>& V2, goSignal3DBase<void>& result)
{
    switch (result.getDataType().getID())
    {
        case GO_INT8: return vectorMult3<T1,T2,goInt8> (V1,V2,result); break;
        case GO_UINT8: return vectorMult3<T1,T2,goUInt8> (V1,V2,result); break;
        case GO_INT16: return vectorMult3<T1,T2,goInt16> (V1,V2,result); break;
        case GO_UINT16: return vectorMult3<T1,T2,goUInt16> (V1,V2,result); break;
        case GO_INT32: return vectorMult3<T1,T2,goInt32> (V1,V2,result); break;
        case GO_UINT32: return vectorMult3<T1,T2,goUInt32> (V1,V2,result); break;
        case GO_FLOAT: return vectorMult3<T1,T2,goFloat> (V1,V2,result); break;
        case GO_DOUBLE: return vectorMult3<T1,T2,goDouble> (V1,V2,result); break;
        default: goLog::warning("goMath::vectorMult(): unknown type for result.");
                 return false; 
                 break;
    }
    return false;
}

    template <class T1>
static inline bool vectorMult1 (const goSignal3DBase<void>& V1, const goSignal3DBase<void>& V2, goSignal3DBase<void>& result)
{
    switch (V2.getDataType().getID())
    {
        case GO_INT8: return vectorMult2<T1,goInt8> (V1,V2,result); break;
        case GO_UINT8: return vectorMult2<T1,goUInt8> (V1,V2,result); break;
        case GO_INT16: return vectorMult2<T1,goInt16> (V1,V2,result); break;
        case GO_UINT16: return vectorMult2<T1,goUInt16> (V1,V2,result); break;
        case GO_INT32: return vectorMult2<T1,goInt32> (V1,V2,result); break;
        case GO_UINT32: return vectorMult2<T1,goUInt32> (V1,V2,result); break;
        case GO_FLOAT: return vectorMult2<T1,goFloat> (V1,V2,result); break;
        case GO_DOUBLE: return vectorMult2<T1,goDouble> (V1,V2,result); break;
        default: goLog::warning("goMath::vectorMult(): unknown type for V2.");
                 return false;
                 break;
    }
    return false;
}

/** 
 * @brief Vector field multiplication.
 *
 * Vectors are stored in the input signals V1 and V2 using
 * the channels provided by goSignal3DBase.
 * For N-vectors, there are of course N channels.
 * The Fields must be of the same size and channel count.
 * The result signal must be of the same size as V1 and V2.
 * All three may have different data types. Keep in mind though that
 * the result will simply be cast to the result type.
 *
 * @todo This function assumes channels are stored linearly in memory.
 * Keep that in mind and change if the channel memory structure should change
 * (which is unlikely).
 * 
 * @param V1 Vector field 1.
 * @param V2 Vector field 2.
 * @param result Result scalar field.
 * 
 * @return True if successful, false otherwise. Check logfile too.
 */
bool goMath::vectorMult (const goSignal3DBase<void>& V1, const goSignal3DBase<void>& V2, goSignal3DBase<void>& result)
{
    if (V1.getChannelCount() != V2.getChannelCount())
    {
        goLog::warning("goMath::vectorMult(): V1 and V2 must have same channel count.");
        return false;
    }
    if (V1.getSizeX() != V2.getSizeX() ||
        V1.getSizeY() != V2.getSizeY() ||
        V1.getSizeZ() != V2.getSizeZ())
    {
        goLog::warning("goMath::vectorMult(): V1 and V2 must have same size.");
        return false;
    }
    if (V1.getSizeX() != result.getSizeX() ||
        V1.getSizeY() != result.getSizeY() ||
        V1.getSizeZ() != result.getSizeZ())
    {
        goLog::warning("goMath::vectorMult(): V1 and V2 and result must have same size.");
        return false;
    }

    switch (V1.getDataType().getID())
    {
        case GO_INT8: return vectorMult1<goInt8> (V1,V2,result); break;
        case GO_UINT8: return vectorMult1<goUInt8> (V1,V2,result); break;
        case GO_INT16: return vectorMult1<goInt16> (V1,V2,result); break;
        case GO_UINT16: return vectorMult1<goUInt16> (V1,V2,result); break;
        case GO_INT32: return vectorMult1<goInt32> (V1,V2,result); break;
        case GO_UINT32: return vectorMult1<goUInt32> (V1,V2,result); break;
        case GO_FLOAT: return vectorMult1<goFloat> (V1,V2,result); break;
        case GO_DOUBLE: return vectorMult1<goDouble> (V1,V2,result); break;
        default: goLog::warning("goMath::vectorMult(): unknown type for V1.");
                 return false;
                 break;
    }
    return false;
}
