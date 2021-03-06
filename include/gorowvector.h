/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GORowVECTOR_H
#define GORowVECTOR_H

#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif

template <class T>
class goRowVector : public goObjectBase 
{
    public:
        goRowVector          (goSignal3DBase<T>* data = NULL);
        virtual ~goRowVector ();

        virtual bool     setData    (goSignal3DBase<T>* data);
        inline  T&       operator[] (goSize_t           x);
        inline  const T& operator[] (goSize_t           x) const;

        void         print      () const;
        
    private:
        goSignal3DBase<T>* myData;
        T*                 myBasePointer;
        const goPtrdiff_t* myJumpTable;

    private:
        goRowVector (const goRowVector<T>& other);
        goRowVector<T>& operator= (goRowVector<T>&);
};

template <class T>
goRowVector<T>::goRowVector (goSignal3DBase<T>* data)
    : goObjectBase  (),
      myData        (NULL),
      myBasePointer (NULL),
      myJumpTable   (NULL)
{
    this->setClassID(GO_ROWVECTOR);
    this->setData (data);
}

template <class T>
goRowVector<T>::~goRowVector ()
{
}

template <class T>
T& goRowVector<T>::operator [] (goSize_t i)
{
    assert (myJumpTable && myBasePointer);
    return *(myJumpTable[i] + myBasePointer);
}

template <class T>
const T& goRowVector<T>::operator [] (goSize_t i) const
{
    assert (myJumpTable && myBasePointer);
    return *(myJumpTable[i] + myBasePointer);
}

template <class T>
bool goRowVector<T>::setData (goSignal3DBase<T>* data)
{
    if (!data)
    {
        myJumpTable   = NULL;
        myBasePointer = NULL;
        myData        = NULL;
        return true;
    }
   
    myData        = data; 
    myJumpTable   = data->getXJump();
    myBasePointer = data->getPtr ();
    return true;
}

template <class T>
void goRowVector<T>::print () const
{
    if (myData)
    {
        GO_SIGNAL3D_EACHELEMENT (std::cout << *__ptr << " ", (*myData), const T);
    }
    std::cout << "\n";
}

#endif
