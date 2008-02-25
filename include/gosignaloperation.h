#ifndef GOSIGNALOPERATION_H

#include <gosignal3dgenericiterator.h>
#include <goautoptr.h>
#include <gofunctor.h>

/** 
 * @brief Generic value-wise operations.
 */
class goSignalOperation
{
    public:
        typedef goAutoPtr<goFunctorBase2<void, goFixedArray<goSignal3DGenericConstIterator>&, goSignal3DGenericIterator&> > KernelMethod;

    public:
        goSignalOperation ();
        virtual ~goSignalOperation ();

        void setKernelMethod (KernelMethod& km);
        void operator () (const goSignal3DBase<void>** sources, goSize_t sourcesCount, goSignal3DBase<void>* target);

    private:
        KernelMethod myKernelMethod;
};

class goSignalOperation2
{
    public:
        typedef goAutoPtr<goFunctorBase1<goDouble, const goDouble&> > KernelMethod;

    public:
        goSignalOperation2 ();
        virtual ~goSignalOperation2 ();

        void setKernelMethod (KernelMethod km);
        void operator () (const goSignal3DBase<void>& source, goSignal3DBase<void>& target);

    private:
        KernelMethod myKernelMethod;
};

#endif
