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
        goFixedArray (goSize_t size = 1, goIndex_t leftBorder = 0, goIndex_t rightBorder = 0) 
            : myArray (0), mySize (0), myLeftBorder (leftBorder), myRightBorder (rightBorder)
        {
            if (size > 0)
                myArray = new T [size + myLeftBorder + myRightBorder];
            if (myArray)
            {
                mySize = size;
                myArray += myLeftBorder;
            }
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
                this->setSize (other.getSize(), other.myLeftBorder, other.myRightBorder);
            }
            if (other.getSize() > 0)
            {
                if (myArray)
                {
                    goIndex_t i;
                    goIndex_t end = static_cast<goIndex_t>(mySize) + myRightBorder;
                    for (i = -myLeftBorder; i < end; ++i)
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
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i];
        };

        T&       operator[] (goIndex_t i)
        {
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i];
        };

        const T& operator() (goIndex_t i) const
        {
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i];
        };

        const T& operator[] (goIndex_t i) const
        {
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i];
        };

        goSize_t  getSize        () const { return mySize; };
        goIndex_t getLeftBorder  () const { return myLeftBorder; };
        goIndex_t getRightBorder () const { return myLeftBorder; };

        void     setSize (goSize_t newSize, goIndex_t leftBorder = 0, goIndex_t rightBorder = 0)
        {
            if (mySize == newSize)
                return;
            if (myArray)
            {
                delete[] (myArray - myLeftBorder);
                myArray = 0;
                mySize = 0;
            }
            myLeftBorder = leftBorder;
            myRightBorder = rightBorder;
            if (newSize > 0)
            {
                myArray = new T [newSize + leftBorder + rightBorder];
                mySize = newSize;
                myArray = myArray + leftBorder;
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
        T*        myArray;
        goSize_t  mySize;
        goIndex_t myLeftBorder;
        goIndex_t myRightBorder;
};
/** @} */
#endif
