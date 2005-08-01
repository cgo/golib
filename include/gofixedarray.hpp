#include <gofixedarray.h>
#include <assert.h>

template <class T>
goFixedArray<T>::goFixedArray (goSize_t size)
    : myArray (0),
      mySize  (0)
{
    myArray = new T [size];
    if (myArray)
        mySize = size;
}

template <class T>
goFixedArray<T>::goFixedArray (const goFixedArray<T>& other)
    : myArray (0),
      mySize  (0)
{
    this->operator= (other);
}

template <class T>
goFixedArray<T>::~goFixedArray ()
{
    if (myArray)
    {
        delete[] myArray;
        myArray = 0;
        mySize = 0;
    }
}

template <class T>
goFixedArray<T>& goFixedArray<T>::operator= (const goFixedArray<T>& other)
{
    if (mySize != other.getSize())
    {
        if (myArray)
        {
            delete[] myArray;
            myArray = 0;
            mySize = 0;
        }
        if (other.getSize() > 0)
        {
            myArray = new T [other.getSize()];
            if (myArray)
            {
                mySize = other.getSize();
                memcpy (myArray, &other(0), sizeof(T) * mySize);
            }
        }
    }
    return *this;
}

template <class T>
T& goFixedArray<T>::operator() (goIndex_t i)
{
    assert (i >= 0 && i < (goIndex_t)mySize);
    return myArray[i];
}

template <class T>
const T& goFixedArray<T>::operator() (goIndex_t i) const
{
    assert (i >= 0 && i < (goIndex_t)mySize);
    return myArray[i];
}

template <class T>
T& goFixedArray<T>::operator[] (goIndex_t i)
{
    assert (i >= 0 && i < (goIndex_t)mySize);
    return myArray[i];
}

template <class T>
const T& goFixedArray<T>::operator[] (goIndex_t i) const
{
    assert (i >= 0 && i < (goIndex_t)mySize);
    return myArray[i];
}

template <class T>
goSize_t goFixedArray<T>::getSize () const
{
    return mySize;
}

template <class T>
void goFixedArray<T>::setSize (goSize_t size)
{
    if (mySize == size)
        return;
           
    if (myArray)
    {
        delete[] myArray;
        myArray = 0;
        mySize = 0;
    }
    myArray = new T [size];
    if (myArray)
        mySize = size;
}
