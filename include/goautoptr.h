#ifndef GOAUTOPTR_H
#define GOAUTOPTR_H

#include <golog.h>

template <class T>
class goRRefPtr
{
    public:
        goRRefPtr (T* p)
            : myRefCount (1), myPtr (p)
        {
        };
        ~goRRefPtr ()
        {
            if (myRefCount > 0)
            {
                goLog::error ("~goRRefPtr(): deleting with refcount > 0");
            }
            if (myPtr)
            {
                // printf ("goRRefPtr deleting myPtr with myRefCount == %d\n", myRefCount);
                delete myPtr;
                myPtr = 0;
            }
        };
        goRRefPtr (goRRefPtr<T>& other)
            : myRefCount (0), myPtr (0)
        {
            *this = other;
        };

        goRRefPtr<T>& operator= (goRRefPtr<T>& other)
        {
            this->myRefCount = other.myRefCount;
            this->myPtr = other.myPtr;
        };

        bool operator== (const goRRefPtr<T>& other) const
        {
            return this->myPtr == other.myPtr;
        };

        bool operator!= (const goRRefPtr<T>& other) const
        {
            return !(*this == other);
        };

        bool operator== (const T* p) const
        {
            return p == myPtr;
        };

        bool operator!= (const T* p) const
        {
            return !(*this == p);
        };
        
        int incRef ()
        {
            ++myRefCount;
            // printf ("refcount == %d\n", myRefCount);
            return myRefCount;
        };
        int decRef ()
        {
            --myRefCount;
            // printf ("refcount == %d\n", myRefCount);
            return myRefCount;
        };
        int getRefCount ()
        {
            return myRefCount;
        };


        int myRefCount;
        T*  myPtr;
};

/**
 * @addtogroup misc
 * @{
 */
/** 
 * @brief "Smart pointer". 
 * Wrapper that automatically deletes its managed pointer when the internal reference
 * count is zero and the last goAutoPtr is deleted.
 */
template <class T>
class goAutoPtr
{
    public:
        /** 
         * @brief Initialises a new pointer and
         * sets the reference count to 1.
         * 
         * @param p Pointer to be managed.
         */
        goAutoPtr (T* p)
            : myRRefPtr (0)
        {
            myRRefPtr = new goRRefPtr<T>(p);
        };

        goAutoPtr ()
            : myRRefPtr (0)
        {
        };

        void set (T* p)
        {
            this->reset ();
            myRRefPtr = new goRRefPtr<T>(p);
        };

        /** 
         * @brief Reset the pointer and reference count.
         * Decrements the current reference count, if a pointer is set,
         * and deletes the pointer if the reference is 0.
         */
        void reset ()
        {
            if (myRRefPtr)
            {
                if (myRRefPtr->decRef() <= 0)
                {
                    delete myRRefPtr;
                    myRRefPtr = 0;
                }
            }
            myRRefPtr = 0;
        };

        /** 
         * @brief Decrements the reference count and, if it is 0,
         * deletes the pointer (if any).
         */
        ~goAutoPtr ()
        {
            // printf ("~goAutoPtr(): myRRefPtr->getRefCount() == %d\n", myRRefPtr->getRefCount());
            if (myRRefPtr)
            {
                if (myRRefPtr->decRef() <= 0)
                {
                    delete myRRefPtr;
                    myRRefPtr = 0;
                }
            }
        };

        /** 
         * @brief Reference the same object as other.
         * 
         * @note The const classifier is only here
         * to ensure that this class works with lists and the like.
         * No RefPtr is const.
         *
         * @param other Other auto ptr.
         */
        goAutoPtr (const goAutoPtr<T>& other)
            : myRRefPtr (0)
        {
            *this = const_cast<goAutoPtr<T>&>(other);
        };

        /** 
        * @brief 
        * 
        * @note The const classifier is only here
        * to ensure that this class works with lists and the like.
        * No RefPtr is const.
        *
        * @param other 
        * 
        * @return 
        */
        goAutoPtr<T>& operator= (const goAutoPtr<T>& other)
        {
            if (other.myRRefPtr == this->myRRefPtr)
            {
                return *this;
            }
            if (myRRefPtr)
            {
                if (myRRefPtr->decRef() <= 0)
                {
                    delete myRRefPtr;
                }
            }
            myRRefPtr = const_cast<goRRefPtr<T>*>(other.myRRefPtr);
            if (myRRefPtr)
            {
                myRRefPtr->incRef ();
            }

            return *this;
        };

        bool isNull () const
        {
            return ((const T*)(*this) == 0);
        };

        bool operator== (const goAutoPtr<T>& other) const
        {
            return myRRefPtr == other.myRRefPtr;
        };

        bool operator!= (const goAutoPtr<T>& other) const
        {
            return !(*this == other);
        };
        
        bool operator== (const T* p) const
        {
            if (!myRRefPtr)
                return !p;
            return *myRRefPtr == p;
        };

        bool operator!= (const T* p) const
        {
            return !(*this == p);
        };
        
        /** 
         * @brief The usual unary operator*().
         * 
         * @return Reference to the managed object.
         */
        T& operator* ()
        {
            //= This yields segfault when myRRefPtr is NULL.
            return *myRRefPtr->myPtr;
        };
        /** 
         * @brief The usual operator->().
         * @return Pointer to the managed object.
         */
        T* operator-> ()
        {
            //= This yields segfault when myRRefPtr is NULL.
            return myRRefPtr->myPtr;
        };

        operator T* () 
        {
            if (!myRRefPtr)
                return 0;
            return myRRefPtr->myPtr;
        };

        operator const T* () const
        {
            if (!myRRefPtr)
                return 0;
            return myRRefPtr->myPtr;
        };

    private:
        goRRefPtr<T>* myRRefPtr;
};
/**
 * @}
 */

#endif
