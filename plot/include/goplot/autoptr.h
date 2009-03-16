#ifndef GOPLOT_AUTOPTR_H
#define GOPLOT_AUTOPTR_H

#include <goplot/plot.h>

namespace NSPACE
{

    class RRefPtrException : public std::exception
    {
        public:
            virtual const char* what() const throw()
            {
                return "RRefPtrException";
            }
    };

    /** 
     * @brief NOT THREAD SAFE!
     */
    template <class T>
        class RRefPtr
        {
            public:
                RRefPtr (T* p)
                    : myRefCount (1), myPtr (p)
                {
                }
                ~RRefPtr ()
                {
                    if (myRefCount > 0)
                    {
                        throw RRefPtrException ();
                    }
                    if (myPtr)
                    {
                        throw RRefPtrException ();
                        //printf ("RRefPtr deleting myPtr with myRefCount == %d\n", myRefCount);
                        //delete myPtr;
                        //myPtr = 0;
                    }
                }

                RRefPtr (RRefPtr<T>& other)
                    : myRefCount (0), myPtr (0)
                {
                    *this = other;
                }

                template <class To>
                    RRefPtr<T>& operator= (RRefPtr<To>& other)
                    {
                        //printf ("RRefPtr = with refcount %d, ptr %p\n", other.myRefCount, other.myPtr);
                        this->myRefCount = other.myRefCount;
                        this->myPtr = static_cast<T*>(other.myPtr);
                    }

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
                    RRefPtr<T>& operator= (const RRefPtr<To>& other)
                    {
                        //printf ("RRefPtr = with refcount %d, ptr %p\n", other.myRefCount, other.myPtr);
                        this->myRefCount = other.myRefCount;
                        this->myPtr = const_cast<T*>(static_cast<const T*>(other.myPtr));
                    };

                template <class To>
                    bool operator== (const RRefPtr<To>& other) const
                    {
                        return this->myPtr == static_cast<T*>(other.myPtr);
                    };

                template <class To>
                    bool operator!= (const RRefPtr<To>& other) const
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
                    ++myRefCount;
                    //printf ("incref: refcount == %d\n", myRefCount);
                    return myRefCount;
                };
                int decRef ()
                {
                    --myRefCount;
                    //printf ("decref: refcount == %d\n", myRefCount);
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
     * @brief "Smart pointer". 
     * Wrapper that automatically deletes its managed pointer when the internal reference
     * count is zero and the last AutoPtr is deleted.
     */
    template <class T>
        class AutoPtr
        {
            public:
                /** 
                 * @brief Initialises a new pointer and
                 * sets the reference count to 1.
                 * 
                 * @param p Pointer to be managed.
                 */
                AutoPtr (T* p)
                    : myRRefPtr (0)
                {
                    myRRefPtr = new RRefPtr<void>(p);
                };

                AutoPtr ()
                    : myRRefPtr (0)
                {
                };

                void set (T* p)
                {
                    this->reset ();
                    myRRefPtr = new RRefPtr<void>((void*)p);
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
                        //printf ("reset decref\n");
                        if (myRRefPtr->decRef() <= 0)
                        {
                            //printf ("reset(): ref <= 0, deleting pointer.\n");
                            //fflush (stdout);
                            delete (T*)myRRefPtr->myPtr;
                            myRRefPtr->myPtr = 0;
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
                ~AutoPtr ()
                {
                    this->reset ();
                    //            // printf ("~AutoPtr(): myRRefPtr->getRefCount() == %d\n", myRRefPtr->getRefCount());
                    //            if (myRRefPtr)
                    //            {
                    //                //printf ("delete operator decref\n");
                    //                if (myRRefPtr->decRef() <= 0)
                    //                {
                    //                    delete myRRefPtr;
                    //                    myRRefPtr = 0;
                    //                }
                    //            }
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
                AutoPtr (const AutoPtr<T>& other)
                    : myRRefPtr (0)
                {
                    *this = const_cast<AutoPtr<T>&>(other);
                };

                template<class To>
                    AutoPtr (const AutoPtr<To>& other)
                    : myRRefPtr (0)
                    {
                        *this = const_cast<AutoPtr<To>&>(other);
                    };

                RRefPtr<void>* getRRefPtr () { return this->myRRefPtr; };
                const RRefPtr<void>* getRRefPtr () const { return this->myRRefPtr; };

                AutoPtr<T>& operator= (const AutoPtr<T>& other)
                {
                    if (other.getRRefPtr() == this->myRRefPtr)
                    {
                        return *this;
                    }
                    this->reset ();
                    //            if (myRRefPtr)
                    //            {
                    //                //printf ("op= decref\n");
                    //                if (myRRefPtr->decRef() <= 0)
                    //                {
                    //                    delete myRRefPtr;
                    //                    myRRefPtr = 0;
                    //                }
                    //            }
                    myRRefPtr = const_cast<RRefPtr<void>*>(other.getRRefPtr()); //= refpointers are never const -- only for compatibility with data structures.
                    if (myRRefPtr)
                    {
                        //printf ("op = incref\n");
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
                    AutoPtr<T>& operator= (const AutoPtr<To>& other)
                    {
                        // asm ("int $3");
                        if (other.getRRefPtr() == this->myRRefPtr)
                        {
                            return *this;
                        }
                        {
                            //= Make the compiler check if a cast is possible.
                            // const T* a = static_cast<const T*> (other.get());
                        }
                        this->reset ();
                        //            if (myRRefPtr)
                        //            {
                        //                //printf ("op= decref\n");
                        //                if (myRRefPtr->decRef() <= 0)
                        //                {
                        //                    delete myRRefPtr;
                        //                    myRRefPtr = 0;
                        //                }
                        //            }
                        myRRefPtr = const_cast<RRefPtr<void>*>(other.getRRefPtr()); //= refpointers are never const -- only for compatibility with data structures.
                        if (myRRefPtr)
                        {
                            //printf ("op = incref\n");
                            myRRefPtr->incRef ();
                        }

                        return *this;
                    };

                bool isNull () const
                {
                    if (!this->myRRefPtr)
                        return true;

                    return (this->myRRefPtr->myPtr == 0);
                    // return ((const T*)(*this) == 0);
                };

                template <class To>
                    bool operator== (const AutoPtr<To>& other) const
                    {
                        return myRRefPtr == other.myRRefPtr;
                    };

                template <class To>
                    bool operator!= (const AutoPtr<To>& other) const
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
                RRefPtr<void>* myRRefPtr;
        };

};
#endif
