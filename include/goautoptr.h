#ifndef GOAUTOPTR_H
#define GOAUTOPTR_H

#include <golog.h>
#include <gothread.h>

template <class T>
class goRRefPtr
{
    public:
        goRRefPtr (T* p)
            : myRefCount (1), myPtr (p), myMutex()
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
                //printf ("goRRefPtr deleting myPtr with myRefCount == %d\n", myRefCount);
                myMutex.lock();
                delete myPtr;
                myPtr = 0;
                myMutex.unlock();
            }
        };
        goRRefPtr (goRRefPtr<T>& other)
            : myRefCount (0), myPtr (0)
        {
            myMutex.lock();
            *this = other;
            myMutex.unlock();
        };

        template <class To>
        goRRefPtr<T>& operator= (goRRefPtr<To>& other)
        {
            myMutex.lock();
            //printf ("goRRefPtr = with refcount %d, ptr %p\n", other.myRefCount, other.myPtr);
            this->myRefCount = other.myRefCount;
            this->myPtr = static_cast<T*>(other.myPtr);
            myMutex.unlock();
        };

        template <class To>
        bool operator== (const goRRefPtr<To>& other) const
        {
            return this->myPtr == static_cast<T*>(other.myPtr);
        };

        template <class To>
        bool operator!= (const goRRefPtr<To>& other) const
        {
            return !(*this == other);
        };

        template <class To>
        bool operator== (const To* p) const
        {
            return p == static_cast<T*>(myPtr);
        };

        template <class To>
        bool operator!= (const To* p) const
        {
            return !(*this == p);
        };
        
        int incRef ()
        {
            myMutex.lock();
            ++myRefCount;
            //printf ("refcount == %d\n", myRefCount);
            myMutex.unlock();
            return myRefCount;
        };
        int decRef ()
        {
            myMutex.lock();
            --myRefCount;
            //printf ("refcount == %d\n", myRefCount);
            myMutex.unlock();
            return myRefCount;
        };
        int getRefCount ()
        {
            return myRefCount;
        };


        int myRefCount;
        T*  myPtr;
        goMutex myMutex;
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

        T* get ()
        {
            if (myRRefPtr)
                return myRRefPtr->myPtr;
            else
                return 0;
        };

        const T* get () const
        {
            if (myRRefPtr)
                return myRRefPtr->myPtr;
            else
                return 0;
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
                //printf ("reset decref\n");
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
                //printf ("delete operator decref\n");
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

        goRRefPtr<T>* getRRefPtr () { return this->myRRefPtr; };
        const goRRefPtr<T>* getRRefPtr () const { return this->myRRefPtr; };
        
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
        // template <class To>
        goAutoPtr<T>& operator= (const goAutoPtr<T>& other)
        {
            if (other.getRRefPtr() == this->myRRefPtr)
            {
                return *this;
            }
            if (myRRefPtr)
            {
                //printf ("op= decref\n");
                if (myRRefPtr->decRef() <= 0)
                {
                    delete myRRefPtr;
                    myRRefPtr = 0;
                }
            }
            myRRefPtr = const_cast<goRRefPtr<T>*>(other.getRRefPtr());
            if (myRRefPtr)
            {
                //printf ("op = incref\n");
                myRRefPtr->incRef ();
            }

            return *this;
        };

        bool isNull () const
        {
            return (this->myRRefPtr == 0 || this->myRRefPtr->myPtr == 0);
            // return ((const T*)(*this) == 0);
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

#if 1
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
#endif

    private:
        goRRefPtr<T>* myRRefPtr;
};
/**
 * @}
 */

#endif
