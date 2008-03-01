#include <gosignaloperation.h>

goSignalOperationGeneric::goSignalOperationGeneric ()
    : myKernelMethod (0)
{
}

goSignalOperationGeneric::~goSignalOperationGeneric ()
{
}

void goSignalOperationGeneric::setKernelMethod (KernelMethod& km)
{
    myKernelMethod = km;
}

void goSignalOperationGeneric::operator () (const goSignal3DBase<void>** sources, goSize_t sourcesCount, goSignal3DBase<void>* target)
{
    goFixedArray<goSignal3DGenericConstIterator> sIts (sourcesCount);
    for (goSize_t i = 0; i < sourcesCount; ++i)
    {
        sIts[i].init (sources[i]);
    }
    // ... use quantization tables for all sources to be generic, and calculate everything in
    // e.g. float. Slow, but generic.
}

//================================================================================ 

template <class T>
goSignalOperation3<T>::goSignalOperation3 ()
    : myKernelMethod (0)
{
}

template <class T>
goSignalOperation3<T>::~goSignalOperation3 ()
{
}

template <class T>
void goSignalOperation3<T>::setKernelMethod (KernelMethod km)
{
    myKernelMethod = km;
}

template <class Ts, class Ts2, class Tt, class T>
static inline void operation3_3 (const goSignal3DBase<void>& source1, const goSignal3DBase<void>& source2, goSignal3DBase<void>& target, typename goSignalOperation3<T>::KernelMethod& km)
{
    goSignal3DGenericConstIterator sIt (&source1);
    goSignal3DGenericConstIterator sIt2 (&source2);
    goSignal3DGenericIterator tIt (&target);

    while (!sIt.endZ())
    {
        sIt.resetY();
        sIt2.resetY();
        tIt.resetY();
        while (!sIt.endY())
        {
            sIt.resetX();
            sIt2.resetX();
            tIt.resetX();
            while (!sIt.endX())
            {
                *(Tt*)*tIt = (Tt) (*km)(*(Ts*)*sIt, *(Ts2*)*sIt2);

                sIt.incrementX ();
                sIt2.incrementX ();
                tIt.incrementX ();
            }
            sIt.incrementY ();
            sIt2.incrementY ();
            tIt.incrementY ();
        }
        sIt.incrementZ ();
        sIt2.incrementZ ();
        tIt.incrementZ ();
    }
}

template <class Ts, class Ts2, class T>
static inline void operation3_2 (const goSignal3DBase<void>& source1, const goSignal3DBase<void>& source2, goSignal3DBase<void>& target, typename goSignalOperation3<T>::KernelMethod& km)
{
    switch (target.getDataType().getID())
    {
        case GO_UINT8: operation3_3 <Ts, Ts2, goUInt8,  T> (source1, source2, target, km); break;
        case GO_INT8: operation3_3  <Ts, Ts2, goInt8,   T> (source1, source2, target, km); break;
        case GO_UINT16: operation3_3<Ts, Ts2, goUInt16, T> (source1, source2, target, km); break;
        case GO_INT16: operation3_3 <Ts, Ts2, goInt16,  T> (source1, source2, target, km); break;
        case GO_UINT32: operation3_3<Ts, Ts2, goUInt32, T> (source1, source2, target, km); break;
        case GO_INT32: operation3_3 <Ts, Ts2, goInt32,  T> (source1, source2, target, km); break;
        case GO_FLOAT: operation3_3 <Ts, Ts2, goFloat,  T> (source1, source2, target, km); break;
        case GO_DOUBLE: operation3_3<Ts, Ts2, goDouble, T> (source1, source2, target, km); break;
        default: goLog::error ("goSignalOperation3::operator(): unknown data type."); break;
    }
}
template <class Ts, class T>
static inline void operation3_1 (const goSignal3DBase<void>& source1, const goSignal3DBase<void>& source2, goSignal3DBase<void>& target, typename goSignalOperation3<T>::KernelMethod& km)
{
    switch (source2.getDataType().getID())
    {
        case GO_UINT8: operation3_2<Ts,goUInt8,T> (source1, source2, target, km); break;
        case GO_INT8: operation3_2<Ts,goInt8,T> (source1, source2, target, km); break;
        case GO_UINT16: operation3_2<Ts,goUInt16,T> (source1, source2, target, km); break;
        case GO_INT16: operation3_2<Ts,goInt16,T> (source1, source2, target, km); break;
        case GO_UINT32: operation3_2<Ts,goUInt32,T> (source1, source2, target, km); break;
        case GO_INT32: operation3_2<Ts,goInt32,T> (source1, source2, target, km); break;
        case GO_FLOAT: operation3_2<Ts,goFloat,T> (source1, source2, target, km); break;
        case GO_DOUBLE: operation3_2<Ts,goDouble,T> (source1, source2, target, km); break;
        default: goLog::error ("goSignalOperation3::operator(): unknown data type."); break;
    }
}

template <class T>
bool goSignalOperation3<T>::operator () (const goSignal3DBase<void>& source1, const goSignal3DBase<void>& source2, goSignal3DBase<void>& target)
{
    switch (source1.getDataType().getID())
    {
        case GO_UINT8: operation3_1<goUInt8,T> (source1, source2, target, myKernelMethod); break;
        case GO_INT8: operation3_1<goInt8,T> (source1, source2, target, myKernelMethod); break;
        case GO_UINT16: operation3_1<goUInt16,T> (source1, source2, target, myKernelMethod); break;
        case GO_INT16: operation3_1<goInt16,T> (source1, source2, target, myKernelMethod); break;
        case GO_UINT32: operation3_1<goUInt32,T> (source1, source2, target, myKernelMethod); break;
        case GO_INT32: operation3_1<goInt32,T> (source1, source2, target, myKernelMethod); break;
        case GO_FLOAT: operation3_1<goFloat,T> (source1, source2, target, myKernelMethod); break;
        case GO_DOUBLE: operation3_1<goDouble,T> (source1, source2, target, myKernelMethod); break;
        default: goLog::error ("goSignalOperation3::operator(): unknown data type."); return false; break;
    }

    return true;
}

// ================================================================================

//================================================================================ 

template <class T>
goSignalOperation2<T>::goSignalOperation2 ()
    : myKernelMethod (0)
{
}

template <class T>
goSignalOperation2<T>::~goSignalOperation2 ()
{
}

template <class T>
void goSignalOperation2<T>::setKernelMethod (KernelMethod km)
{
    myKernelMethod = km;
}

template <class Ts, class Tt, class T>
static inline void operation2 (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, typename goSignalOperation2<T>::KernelMethod& km)
{
    goSignal3DGenericConstIterator sIt (&source);
    goSignal3DGenericIterator tIt (&target);

    while (!sIt.endZ())
    {
        sIt.resetY();
        tIt.resetY();
        while (!sIt.endY())
        {
            sIt.resetX();
            tIt.resetX();
            while (!sIt.endX())
            {
                *(Tt*)*tIt = (Tt) (*km)(*(Ts*)*sIt);

                sIt.incrementX ();
                tIt.incrementX ();
            }
            sIt.incrementY ();
            tIt.incrementY ();
        }
        sIt.incrementZ ();
        tIt.incrementZ ();
    }
}

template <class Ts, class T>
static inline void operation1 (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, typename goSignalOperation2<T>::KernelMethod& km)
{
    switch (target.getDataType().getID())
    {
        case GO_UINT8: operation2<Ts,goUInt8,T> (source, target, km); break;
        case GO_INT8: operation2<Ts,goInt8,T> (source, target, km); break;
        case GO_UINT16: operation2<Ts,goUInt16,T> (source, target, km); break;
        case GO_INT16: operation2<Ts,goInt16,T> (source, target, km); break;
        case GO_UINT32: operation2<Ts,goUInt32,T> (source, target, km); break;
        case GO_INT32: operation2<Ts,goInt32,T> (source, target, km); break;
        case GO_FLOAT: operation2<Ts,goFloat,T> (source, target, km); break;
        case GO_DOUBLE: operation2<Ts,goDouble,T> (source, target, km); break;
        default: goLog::error ("goSignalOperation2::operator(): unknown data type."); break;
    }
}

template <class T>
bool goSignalOperation2<T>::operator () (const goSignal3DBase<void>& source, goSignal3DBase<void>& target)
{
    switch (source.getDataType().getID())
    {
        case GO_UINT8: operation1<goUInt8,T> (source, target, myKernelMethod); break;
        case GO_INT8: operation1<goInt8,T> (source, target, myKernelMethod); break;
        case GO_UINT16: operation1<goUInt16,T> (source, target, myKernelMethod); break;
        case GO_INT16: operation1<goInt16,T> (source, target, myKernelMethod); break;
        case GO_UINT32: operation1<goUInt32,T> (source, target, myKernelMethod); break;
        case GO_INT32: operation1<goInt32,T> (source, target, myKernelMethod); break;
        case GO_FLOAT: operation1<goFloat,T> (source, target, myKernelMethod); break;
        case GO_DOUBLE: operation1<goDouble,T> (source, target, myKernelMethod); break;
        default: goLog::error ("goSignalOperation2::operator(): unknown data type."); return false; break;
    }

    return true;
}

// ================================================================================

template <class T>
goSignalOperation1<T>::goSignalOperation1 ()
    : myKernelMethod (0)
{
}

template <class T>
goSignalOperation1<T>::~goSignalOperation1 ()
{
}

template <class T>
void goSignalOperation1<T>::setKernelMethod (KernelMethod km)
{
    myKernelMethod = km;
}

template <class Ts, class T>
static inline void operation1_1 (goSignal3DBase<void>& sig, typename goSignalOperation1<T>::KernelMethod& km)
{
    goSignal3DGenericIterator sIt (&sig);

    Ts* p = 0;
    while (!sIt.endZ())
    {
        sIt.resetY();
        while (!sIt.endY())
        {
            sIt.resetX();
            while (!sIt.endX())
            {
                p = (Ts*)*sIt;
                *p = (Ts) (*km)(*p);

                sIt.incrementX ();
            }
            sIt.incrementY ();
        }
        sIt.incrementZ ();
    }
}
template <class T>
bool goSignalOperation1<T>::operator () (goSignal3DBase<void>& sig)
{
    switch (sig.getDataType().getID())
    {
        case GO_UINT8: operation1_1<goUInt8,T> (sig, myKernelMethod); break;
        case GO_INT8: operation1_1<goInt8,T> (sig, myKernelMethod); break;
        case GO_UINT16: operation1_1<goUInt16,T> (sig, myKernelMethod); break;
        case GO_INT16: operation1_1<goInt16,T> (sig, myKernelMethod); break;
        case GO_UINT32: operation1_1<goUInt32,T> (sig, myKernelMethod); break;
        case GO_INT32: operation1_1<goInt32,T> (sig, myKernelMethod); break;
        case GO_FLOAT: operation1_1<goFloat,T> (sig, myKernelMethod); break;
        case GO_DOUBLE: operation1_1<goDouble,T> (sig, myKernelMethod); break;
        default: goLog::error ("goSignalOperation2::operator(): unknown data type."); return false; break;
    }

    return true;
}

// ================================================================================

template <class T>
goSignalOperation1Const<T>::goSignalOperation1Const ()
    : myKernelMethod (0)
{
}

template <class T>
goSignalOperation1Const<T>::~goSignalOperation1Const ()
{
}

template <class T>
void goSignalOperation1Const<T>::setKernelMethod (KernelMethod km)
{
    myKernelMethod = km;
}

template <class Ts, class T>
static inline void operation1_1_const (const goSignal3DBase<void>& sig, typename goSignalOperation1Const<T>::KernelMethod& km)
{
    goSignal3DGenericConstIterator sIt (&sig);

    while (!sIt.endZ())
    {
        sIt.resetY();
        while (!sIt.endY())
        {
            sIt.resetX();
            while (!sIt.endX())
            {
                (*km)(*(Ts*)*sIt);
                sIt.incrementX ();
            }
            sIt.incrementY ();
        }
        sIt.incrementZ ();
    }
}

template <class T>
bool goSignalOperation1Const<T>::operator () (const goSignal3DBase<void>& sig)
{
    switch (sig.getDataType().getID())
    {
        case GO_UINT8: operation1_1_const<goUInt8,T> (sig, myKernelMethod); break;
        case GO_INT8: operation1_1_const<goInt8,T> (sig, myKernelMethod); break;
        case GO_UINT16: operation1_1_const<goUInt16,T> (sig, myKernelMethod); break;
        case GO_INT16: operation1_1_const<goInt16,T> (sig, myKernelMethod); break;
        case GO_UINT32: operation1_1_const<goUInt32,T> (sig, myKernelMethod); break;
        case GO_INT32: operation1_1_const<goInt32,T> (sig, myKernelMethod); break;
        case GO_FLOAT: operation1_1_const<goFloat,T> (sig, myKernelMethod); break;
        case GO_DOUBLE: operation1_1_const<goDouble,T> (sig, myKernelMethod); break;
        default: goLog::error ("goSignalOperation2::operator(): unknown data type."); return false; break;
    }

    return true;
}

template class goSignalOperation1<goFloat>;
template class goSignalOperation1<goDouble>;
template class goSignalOperation1Const<goFloat>;
template class goSignalOperation1Const<goDouble>;

template class goSignalOperation2<goFloat>;
template class goSignalOperation2<goDouble>;

template class goSignalOperation3<goFloat>;
template class goSignalOperation3<goDouble>;
