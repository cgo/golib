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
 * @brief Array class.
 *
 * This array can be used as a replacement for simple
 * C++ arrays.
 * As opposed to goArray<>, it can be used with any data type
 * that offers new, delete, and = operators.
 *
 * goArray uses malloc and can resize memory using the C library functions.
 * goFixedArray also supports resize, but copies data when resizing and the size is larger than
 * the number of reserved elements.
 *
 * To resize an array, use the \c resize() method. It copies the old content to the new array, if the 
 * array must be reallocated. It is only reallocated if the new size is larger than the reserved size.
 * Use the \c setSize() method to allocate newly without copying.
 *
 * @todo resize() currently does not shrink memory usage when resizing to a smaller size.
 * This should possibly be changed as it may lead to unexpectedly large memory consumption of
 * programs.
 *
 * @author Christian Gosch
 */
template <class T> class goFixedArray
{
    public:
        /** 
         * @brief Constructor.
         *
         * Makes an array of size \c size, with reserved memory for \c reserve elements in total
         * and \c resize_overhead overhead elements (see getReserved() and getResizeOverhead()).
         * If reserve is lower than size, \c size elements are reserved.
         *
         * @param size Size in number elements.
         * @param reserve Reserved memory in number of elements (default 0)
         * @param resize_overhead Resize overhead in number of elements (default 0)
         */
        explicit goFixedArray (goSize_t size = 1, goSize_t reserve = 0, goSize_t resize_overhead = 0) 
            : myArray (0), 
              mySize (0), 
              myReserved (0),
              myResizeOverhead (resize_overhead),
              myStride(1), 
              myExternal(false)
        {
            if (size > 0)
                myArray = new T [size > reserve ? size : reserve];
            if (myArray)
            {
                mySize = size;
                myReserved = size > reserve ? size : reserve;
            }
        }

        goFixedArray (const goFixedArray<T>& other) 
            : myArray (0), mySize (0), myReserved (0), myResizeOverhead (0), myStride(1), 
              myExternal(false)
        {
            this->operator= (other);
        }
#if 0
        template <class To>
        goFixedArray (const goFixedArray<To>& other) 
            : myArray (0), mySize (0), myLeftBorder (0), myRightBorder (0), myStride(1), myExternal(false)
        {
            this->operator= (other);
        };
#endif
        goFixedArray (T* ptr, goSize_t size, goIndex_t stride)
            : myArray (ptr), 
              mySize (size), 
              myReserved (size),
              myResizeOverhead (0),
              myStride (stride), 
              myExternal (true)
        {
        }

        virtual ~goFixedArray ()
        {
            if (myArray && !myExternal)
            {
                delete[] myArray;
                myArray = 0;
                mySize = 0;
                myReserved = 0;
            }
        }

//        template <class To>
        goFixedArray<T>& operator= (const goFixedArray<T>& other)
        {
            if (mySize != other.getSize())
            {
                this->setSize (other.getSize(), other.getReserved(), other.getResizeOverhead ());
            }
            if (other.getSize() > 0)
            {
                if (myArray)
                {
                    for (goSize_t i = 0; i < mySize; ++i)
                    {
                        (*this)(i) = other (i);
                    }
                }
            }
            return *this;
        }

        void setData (T* ptr, goSize_t size, goIndex_t stride)
        {
            if (!this->myExternal && this->myArray)
            {
                delete[] this->myArray;
                this->myArray = 0;
                this->mySize = 0;
                this->myReserved = 0;
            }
            this->myExternal = true;
            this->myArray    = ptr;
            this->myStride   = stride;
            this->mySize     = size;
            this->myReserved = size;
        }

        void ref (goFixedArray<T>& target, goSize_t startIndex = 0, goSize_t size = 0)
        {
            if (size == 0)
            {
                target.setData (this->getPtr() + startIndex * this->getStride(), this->getSize() - startIndex, this->getStride());
            }
            else
            {
                target.setData (this->getPtr() + startIndex * this->getStride(), size, this->getStride());
            }
        }

        T&       operator() (goIndex_t i)
        {
            //assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        }

        T&       operator[] (goIndex_t i)
        {
            //assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        }

        const T& operator() (goIndex_t i) const
        {
            //assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        }

        const T& operator[] (goIndex_t i) const
        {
            //assert (i >= -myLeftBorder && i < (goIndex_t)mySize + myRightBorder);
            return myArray[i * myStride];
        }

        /** 
         * @brief Get the size of the array in number of elements.
         * 
         * @note This is not necessarily equal to the number of allocated elements.
         * The number of actually reserved elements can be retrieved with getReserved().
         *
         * @return Size of the array in number of elements.
         */
        goSize_t  getSize        () const { return mySize; }
        
        /** 
         * @brief Get the number of actually allocated elements.
         *
         * Get the number of elements for which memory was allocated. The \b size 
         * of the array is always lower than or equal to the number of reserved elements.
         *
         * This is done in order to allow for quicker appending of new elements
         * in situations where that is necessary.
         *
         * This value can be set by the constructor or by \c setSize().
         *
         * @return Number of elements for which memory was allocated.
         */
        goSize_t  getReserved    () const { return myReserved; }

        /** 
         * @brief Get the number of resize overhead elements.
         * 
         * The "resize overhead" is a number of elements
         * that is used in case of a \c resize() if the new size is larger than the 
         * number of reserved elements (see \c getReserved()).
         * In that case, memory for \c newSize + \c getResizeOverhead()
         * elements will be allocated while the size will be \c newSize (see \c resize()).<br>
         * The default is 0.
         *
         * @return Number of overhead elements.
         */
        goSize_t  getResizeOverhead () const { return myResizeOverhead; }
        //goIndex_t getLeftBorder  () const { return myLeftBorder; };
        //goIndex_t getRightBorder () const { return myLeftBorder; };
        goIndex_t getStride      () const { return myStride; };

        /** 
         * @brief Set the resize overhead in number of elements.
         * 
         * @see getResizeOverhead()
         *
         * @param ro The new resize overhead in number of elements.
         */
        void setResizeOverhead (goSize_t ro) { myResizeOverhead = ro; }

        /** 
         * @brief Set the size of the array, deleting old content.
         *
         * @see resize()
         *
         * If the size is differing from the old size of the array, the array
         * is deleted and new memory is allocated. Contents are lost.
         * Use \c resize() for a resize that copies the content automatically.
         *
         * @param newSize New size in number of elements.
         * @param reserve Number of elements to reserve memory for (see also \c getReserved()).
         * @param resize_overhead Number of resize overhead elements (see also \c getResizeOverhead()).
         */
        void     setSize (goSize_t newSize, goSize_t reserve = 0, goSize_t resize_overhead = 0)
        {
            if (mySize == newSize)
                return;

            if (myArray)
            {
                if (!myExternal)
                {
                    delete[] myArray;
                }
                myArray = 0;
                mySize = 0;
                myResizeOverhead = 0;
                myReserved = 0;
            }

            myReserved = newSize > reserve ? newSize : reserve;
            myResizeOverhead = resize_overhead;

            if (myReserved > 0)
            {
                myArray = new T [myReserved];
                mySize = newSize;
                myStride = 1;
                myExternal = false;
            }
        }

        /** 
         * @brief Proper resize with copying.
         * 
         * New space will be available at the \b end of the array.
         *
         * If \c newSize is larger than the current reserved space as returned by getReserved(),
         * new memory with size \c newSize + \c getResizeOverhead() will be allocated,
         * the old content will be copied and the size is set to \c newSize, reserved is set
         * to \c newSize + \c getResizeOverhead().
         *
         * If \c newSize is lower than \c getReserved() - 2 * \c getResizeOverhead(), the array is also
         * reallocated to \c newSize + \c getResizeOverhead() and the contents are copied.
         *
         * In all other cases, the size is simply adjusted.
         *
         * @param newSize New size of the array in number of elements.
         * 
         * @return True if successful, false otherwise.
         */
        bool resize (goSize_t newSize)
        {
            if (newSize > myReserved || newSize < (myReserved - 2 * myResizeOverhead))
            {
                //= Copy the old stuff
                T* temp = new T [newSize + myResizeOverhead];

                for (goSize_t i = 0; i < mySize; ++i)
                {
                    temp [i] = myArray [i];
                }

                myReserved = newSize + myResizeOverhead;
                mySize = newSize;
                
                T* temptemp = myArray;
                myArray = temp;
                if (temptemp)
                {
                    delete [] temptemp;
                    temptemp = 0;
                }
            }
            else
            {
                mySize = newSize;
            }

            return true;
        }

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
        }
        
        bool operator!= (const goFixedArray<T>& other) const
        {
            return !this->operator==(other);  
        }
        
        /** 
         * @brief Get the pointer to the data.
         * 
         * @return The pointer to the data.
         */
        T*       getPtr  () { return myArray; }

        /** 
         * @brief Get the pointer to the data.
         * 
         * @return The pointer to the data.
         */
        const T* getPtr  () const { return myArray; }

        /** 
         * @brief Fill the array with \c value.
         * 
         * @param value The new value.
         */
        void fill (const T& value)
        {
            goIndex_t size = (goIndex_t)mySize;
            T* array = myArray;
            for (goIndex_t i = 0; i < size; ++i, array += myStride)
            {
                *array = value;
            }
        }
       
        /** 
         * @brief Flip the direction of the data.
         *
         * Actually \b copies the data.
         * 
         * @TODO: An alternative is to just change the stride and start pointer.
         */
        void flip ()
        {
            goSize_t sz = this->getSize();
            goSize_t j = sz - 1;
            T temp;
            for (goSize_t i = 0; i < j; ++i, --j)
            {
                temp = (*this)[i];
                (*this)[i] = (*this)[j];
                (*this)[j] = temp;
            }
        }

    private:
        T*        myArray;
        goSize_t  mySize;
        goSize_t  myReserved;
        goSize_t  myResizeOverhead;
        //goIndex_t myLeftBorder;
        //goIndex_t myRightBorder;
        goIndex_t myStride;
        bool      myExternal;
};
/** @} */
#endif
