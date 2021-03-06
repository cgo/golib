/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
                goLog::error ("~goRRefPtr(): deleting with myPtr != 0");
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
            this->myRefCount = other.myRefCount;
            this->myPtr = static_cast<T*>(other.myPtr);
            myMutex.unlock();
        };

        /*
         * @brief 
         * 
         * The refpointers are never const -- this is only here so that autoptr and friends work with lists
         * and other structures.
         *
         * @param other 
         * 
         * @return 
         */
        template <class To>
        goRRefPtr<T>& operator= (const goRRefPtr<To>& other)
        {
            myMutex.lock();
            this->myRefCount = other.myRefCount;
            this->myPtr = const_cast<T*>(static_cast<const T*>(other.myPtr));
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
            myMutex.unlock();
            return myRefCount;
        };
        int decRef ()
        {
            myMutex.lock();
            --myRefCount;
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
            myRRefPtr = new goRRefPtr<void>(p);
        };

        goAutoPtr ()
            : myRRefPtr (0)
        {
        };

        void set (T* p)
        {
            this->reset ();
            myRRefPtr = new goRRefPtr<void>((void*)p);
        };

        T* get ()
        {
            if (myRRefPtr)
                return (T*)myRRefPtr->myPtr;
            else
                return 0;
        };

        const T* get () const
        {
            if (myRRefPtr)
                return (const T*)myRRefPtr->myPtr;
            else
                return (const T*)0;
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
                    myRRefPtr->myMutex.lock();
                    delete (T*)myRRefPtr->myPtr;
                    myRRefPtr->myPtr = 0;
                    myRRefPtr->myMutex.unlock();
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
            this->reset ();
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

        template<class To>
        goAutoPtr (const goAutoPtr<To>& other)
            : myRRefPtr (0)
        {
            *this = const_cast<goAutoPtr<To>&>(other);
        };

        goRRefPtr<void>* getRRefPtr () { return this->myRRefPtr; };
        const goRRefPtr<void>* getRRefPtr () const { return this->myRRefPtr; };
        
        goAutoPtr<T>& operator= (const goAutoPtr<T>& other)
        {
            if (other.getRRefPtr() == this->myRRefPtr)
            {
                return *this;
            }
            this->reset ();
            myRRefPtr = const_cast<goRRefPtr<void>*>(other.getRRefPtr()); //= refpointers are never const -- only for compatibility with data structures.
            if (myRRefPtr)
            {
                myRRefPtr->incRef ();
            }

            return *this;
        };
        /** 
        * @brief 
        * 
        * @note The const classifier is only here
        * to ensure that this class works with lists and the like.
        * No RefPtr is const.
        *
        * @bug SEE CODE!
        *
        * @param other 
        * 
        * @return 
        */
        template <class To>
        goAutoPtr<T>& operator= (const goAutoPtr<To>& other)
        {
            // asm ("int $3");
            if (other.getRRefPtr() == this->myRRefPtr)
            {
                return *this;
            }
            this->reset ();
            myRRefPtr = const_cast<goRRefPtr<void>*>(other.getRRefPtr()); //= refpointers are never const -- only for compatibility with data structures.
            if (myRRefPtr)
            {
	      myRRefPtr->incRef ();
            }

            return *this;
        };

        bool isNull () const
        {
            return (this->myRRefPtr == 0 || this->myRRefPtr->myPtr == 0);
        };

        template <class To>
        bool operator== (const goAutoPtr<To>& other) const
        {
            return myRRefPtr == other.myRRefPtr;
        };

        template <class To>
        bool operator!= (const goAutoPtr<To>& other) const
        {
            return !(*this == other);
        };
        
        bool operator== (const T* p) const
        {
            if (!myRRefPtr)
                return !p;
            return (T*)myRRefPtr->myPtr == p;
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
            return *(T*)myRRefPtr->myPtr;
        };
        /** 
         * @brief The usual operator->().
         * @return Pointer to the managed object.
         */
        T* operator-> ()
        {
            //= This yields segfault when myRRefPtr is NULL.
            return (T*)myRRefPtr->myPtr;
        };

#if 1
        operator T* () 
        {
            if (!myRRefPtr)
                return 0;
            return (T*)myRRefPtr->myPtr;
        };

        operator const T* () const
        {
            if (!myRRefPtr)
                return 0;
            return (const T*)myRRefPtr->myPtr;
        };
#endif

    private:
        goRRefPtr<void>* myRRefPtr;
};
/**
 * @}
 */

#endif
