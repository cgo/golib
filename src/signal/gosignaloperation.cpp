#include <gosignaloperation.h>

goSignalOperation::goSignalOperation ()
    : myKernelMethod (0)
{
}

goSignalOperation::~goSignalOperation ()
{
}

void goSignalOperation::setKernelMethod (KernelMethod& km)
{
    myKernelMethod = km;
}

void goSignalOperation::operator () (const goSignal3DBase<void>** sources, goSize_t sourcesCount, goSignal3DBase<void>* target)
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

goSignalOperation2::goSignalOperation2 ()
    : myKernelMethod (0)
{
}

goSignalOperation2::~goSignalOperation2 ()
{
}

void goSignalOperation2::setKernelMethod (KernelMethod km)
{
    myKernelMethod = km;
}

template <class Ts, class Tt>
static inline void operation2 (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, goSignalOperation2::KernelMethod& km)
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

template <class Ts>
static inline void operation1 (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, goSignalOperation2::KernelMethod& km)
{
    switch (target.getDataType().getID())
    {
        case GO_UINT8: operation2<Ts,goUInt8> (source, target, km); break;
        case GO_INT8: operation2<Ts,goInt8> (source, target, km); break;
        case GO_UINT16: operation2<Ts,goUInt16> (source, target, km); break;
        case GO_INT16: operation2<Ts,goInt16> (source, target, km); break;
        case GO_UINT32: operation2<Ts,goUInt32> (source, target, km); break;
        case GO_INT32: operation2<Ts,goInt32> (source, target, km); break;
        case GO_FLOAT: operation2<Ts,goFloat> (source, target, km); break;
        case GO_DOUBLE: operation2<Ts,goDouble> (source, target, km); break;
        default: goLog::error ("goSignalOperation2::operator(): unknown data type."); break;
    }
}

void goSignalOperation2::operator () (const goSignal3DBase<void>& source, goSignal3DBase<void>& target)
{
    switch (source.getDataType().getID())
    {
        case GO_UINT8: operation1<goUInt8> (source, target, myKernelMethod); break;
        case GO_INT8: operation1<goInt8> (source, target, myKernelMethod); break;
        case GO_UINT16: operation1<goUInt16> (source, target, myKernelMethod); break;
        case GO_INT16: operation1<goInt16> (source, target, myKernelMethod); break;
        case GO_UINT32: operation1<goUInt32> (source, target, myKernelMethod); break;
        case GO_INT32: operation1<goInt32> (source, target, myKernelMethod); break;
        case GO_FLOAT: operation1<goFloat> (source, target, myKernelMethod); break;
        case GO_DOUBLE: operation1<goDouble> (source, target, myKernelMethod); break;
        default: goLog::error ("goSignalOperation2::operator(): unknown data type."); break;
    }
}

