#ifndef GOFIXEDARRAY_H
#define GOFIXEDARRAY_H

#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#include <assert.h>

/** @addtogroup data
 * @{
 */
/** 
 * @brief Fixed length array.
 *
 * This array can be used as a replacement for simple
 * C++ arrays.
 * As opposed to goArray<>, it can be used with any data type
 * that offers new, delete, and = operators.
 * 
 * @author Christian Gosch
 */
template <class T> class goFixedArray
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
                    goSize_t i;
                    for (i = 0; i < mySize; ++i)
                    {
                        myArray[i] = other(i);
                    }
                    // memcpy (myArray, &other(0), sizeof(T) * mySize);
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

        bool operator== (const goFixedArray<T>& other) const
        {
            if (this->getSize() != other.getSize())
                return false;
            goSize_t i;
            const T* oP = other.getPtr();
            const T* mP = this->getPtr();
            for (i = 0; i < mySize; ++i,++oP,++mP)
            {
                if (*oP != *mP)
                    return false;
            }
            return true;
        };
        
        bool operator!= (const goFixedArray<T>& other) const
        {
            return !this->operator==(other);  
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
/** @} */
#endif
