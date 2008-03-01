#ifndef GOSIGNALOPERATION_H

#include <gosignal3dgenericiterator.h>
#include <goautoptr.h>
#include <gofunctor.h>

// Currently unused
class goSignalOperation
{
    public:
        virtual ~goSignalOperation ();

        virtual void operator () () = 0;

    protected:
        goSignalOperation ();
};

/** 
 * @brief Generic value-wise operations.
 * @note Not yet implemented, just a stub.
 */
class goSignalOperationGeneric
{
    public:
        typedef goAutoPtr<goFunctorBase2<void, goFixedArray<goSignal3DGenericConstIterator>&, goSignal3DGenericIterator&> > KernelMethod;

    public:
        goSignalOperationGeneric ();
        virtual ~goSignalOperationGeneric ();

        void setKernelMethod (KernelMethod& km);
        void operator () (const goSignal3DBase<void>** sources, goSize_t sourcesCount, goSignal3DBase<void>* target);

    private:
        KernelMethod myKernelMethod;
};

template <class T>
class goSignalOperation3
{
    public:
        typedef goAutoPtr<goFunctorBase2<T, T, T> > KernelMethod;

    public:
        goSignalOperation3 ();
        virtual ~goSignalOperation3 ();

        void setKernelMethod (KernelMethod km);
        bool operator () (const goSignal3DBase<void>& source1, const goSignal3DBase<void>& source2, goSignal3DBase<void>& target);

    private:
        KernelMethod myKernelMethod;
};

template <class T>
class goSignalOperation2
{
    public:
        typedef goAutoPtr<goFunctorBase1<T, T> > KernelMethod;

    public:
        goSignalOperation2 ();
        virtual ~goSignalOperation2 ();

        void setKernelMethod (KernelMethod km);
        bool operator () (const goSignal3DBase<void>& source, goSignal3DBase<void>& target);

    private:
        KernelMethod myKernelMethod;
};

template <class T>
class goSignalOperation1
{
    public:
        typedef goAutoPtr<goFunctorBase1<T, T> > KernelMethod;

    public:
        goSignalOperation1 ();
        virtual ~goSignalOperation1 ();

        void setKernelMethod (KernelMethod km);
        bool operator () (goSignal3DBase<void>& sig);

    private:
        KernelMethod myKernelMethod;
};

template <class T>
class goSignalOperation1Const
{
    public:
        typedef goAutoPtr<goFunctorBase1<T, T> > KernelMethod;

    public:
        goSignalOperation1Const ();
        virtual ~goSignalOperation1Const ();

        void setKernelMethod (KernelMethod km);
        bool operator () (const goSignal3DBase<void>& sig);

    private:
        KernelMethod myKernelMethod;
};

#endif
