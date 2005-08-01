#ifndef GOFIXEDARRAY_H
#define GOFIXEDARRAY_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#include <assert.h>

template <class T>
class goFixedArray
{
    public:
        goFixedArray (goSize_t size = 1) : myArray (0), mySize (0)
        {
            if (size > 0)
                myArray = new T [size];
            if (myArray)
                mySize = size;
        };
        goFixedArray (const goFixedArray<T>& other) : myArray (0), mySize (0)
        {
            this->operator= (other);
        };
        virtual ~goFixedArray ()
        {
            if (myArray)
            {
                delete[] myArray;
                myArray = 0;
                mySize = 0;
            }
        };
        goFixedArray<T>& operator= (const goFixedArray<T>& other)
        {
            if (mySize != other.getSize())
            {
                if (myArray)
                {
                    delete[] myArray;
                    myArray = 0;
                    mySize = 0;
                }
                myArray = new T [other.getSize()];
                mySize = other.getSize();
            }
            if (other.getSize() > 0)
            {
                if (myArray)
                {
                    // goIndex_t i;
                    //for (i = 0; i < mySize; ++i)
                    //{
                    //    myArray[i] = other(i);
                    //}
                    memcpy (myArray, &other(0), sizeof(T) * mySize);
                }
            }
            return *this;
        };

        T&       operator() (goIndex_t i)
        {
            assert (i >= 0 && i < (goIndex_t)mySize);
            return myArray[i];
        };

        T&       operator[] (goIndex_t i)
        {
            assert (i >= 0 && i < (goIndex_t)mySize);
            return myArray[i];
        };

        const T& operator() (goIndex_t i) const
        {
            assert (i >= 0 && i < (goIndex_t)mySize);
            return myArray[i];
        };

        const T& operator[] (goIndex_t i) const
        {
            assert (i >= 0 && i < (goIndex_t)mySize);
            return myArray[i];
        };

        goSize_t getSize () const { return mySize; };

        void     setSize (goSize_t newSize)
        {
            // asm("int $3");
            if (mySize == newSize)
                return;
            if (myArray)
            {
                delete[] myArray;
                myArray = 0;
                mySize = 0;
            }
            if (newSize > 0)
            {
                myArray = new T [newSize];
                mySize = newSize;
            }
        };

        T*       getPtr  () { return myArray; };
        const T* getPtr  () const { return myArray; };

        void fill (const T& value)
        {
            goIndex_t size = (goIndex_t)mySize;
            T* array = myArray;
            for (goIndex_t i = 0; i < size; ++i, ++array)
            {
                *array = value;
            }
        };
        
    private:
        T*       myArray;
        goSize_t mySize;
};

#endif
