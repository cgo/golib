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
            : myArray (0), mySize (0), myLeftBorder (leftBorder), myRightBorder (rightBorder), myStride(1), myExternal(false)
        {
            if (size > 0)
                myArray = new T [size + myLeftBorder + myRightBorder];
            if (myArray)
            {
                mySize = size;
                myArray += myLeftBorder;
            }
        };

        goFixedArray (const goFixedArray<T>& other) 
            : myArray (0), mySize (0), myLeftBorder (0), myRightBorder (0), myStride(1), myExternal(false)
        {
            this->operator= (other);
        };
#if 0
        template <class To>
        goFixedArray (const goFixedArray<To>& other) 
            : myArray (0), mySize (0), myLeftBorder (0), myRightBorder (0), myStride(1), myExternal(false)
        {
            this->operator= (other);
        };
#endif
        goFixedArray (T* ptr, goSize_t size, goIndex_t stride)
            : myArray (ptr), mySize (size), myLeftBorder (0), myRightBorder (0), myStride(stride), myExternal(true)
        {
        };

        virtual ~goFixedArray ()
        {
            if (myArray && !myExternal)
            {
                delete[] (myArray - myLeftBorder);
                myArray = 0;
                mySize = 0;
            }
        };

//        template <class To>
        goFixedArray<T>& operator= (const goFixedArray<T>& other)
        {
            if (mySize != other.getSize())
            {
                this->setSize (other.getSize(), other.getLeftBorder(), other.getRightBorder());
            }
            if (other.getSize() > 0)
            {
                if (myArray)
                {
                    goIndex_t i;
                    goIndex_t end = static_cast<goIndex_t>(mySize) + myRightBorder;
                    for (i = -myLeftBorder; i < end; ++i)
                    {
                        (*this)(i) = other(i);
                    }
                }
            }
            return *this;
        };

        void setData (T* ptr, goSize_t size, goIndex_t stride)
        {
            if (!this->myExternal && this->myArray)
            {
                delete[] (this->myArray - this->myLeftBorder);
                this->myArray = 0;
                this->mySize = 0;
            }
            this->myExternal = true;
            this->myArray    = ptr;
            this->myStride   = stride;
            this->mySize     = size;
        };

        T&       operator() (goIndex_t i)
        {
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        };

        T&       operator[] (goIndex_t i)
        {
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        };

        const T& operator() (goIndex_t i) const
        {
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        };

        const T& operator[] (goIndex_t i) const
        {
            assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        };

        goSize_t  getSize        () const { return mySize; };
        goIndex_t getLeftBorder  () const { return myLeftBorder; };
        goIndex_t getRightBorder () const { return myLeftBorder; };
        goIndex_t getStride      () const { return myStride; };

        void     setSize (goSize_t newSize, goIndex_t leftBorder = 0, goIndex_t rightBorder = 0)
        {
            if (mySize == newSize)
                return;
            if (myArray)
            {
                if (!myExternal)
                {
                    delete[] (myArray - myLeftBorder);
                }
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
                myStride = 1;
                myExternal = false;
            }
        };

        bool operator== (const goFixedArray<T>& other) const
        {
            if (this->getSize() != other.getSize())
                return false;
            goSize_t i;
            for (i = 0; i < mySize; ++i)
            {
                if ((*this)(i) != other(i))
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
            for (goIndex_t i = 0; i < size; ++i, array += myStride)
            {
                *array = value;
            }
        };
        
    private:
        T*        myArray;
        goSize_t  mySize;
        goIndex_t myLeftBorder;
        goIndex_t myRightBorder;
        goIndex_t myStride;
        bool      myExternal;
};
/** @} */
#endif
