#ifndef GOFUNCTOR_H
#define GOFUNCTOR_H

#include <goautoptr.h>
#include <golist.h>
#include <golog.h>

#include "gofunctor2.h"


#if 0
/** 
 * @brief Virtual base class for functors with 1 argument.
 */
template <class Tret, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9, class Targ10>
class goFunctorBase
{
    public:
        goFunctorBase ()
        {
        };
        virtual ~goFunctorBase ()
        {
        };

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        // virtual Tret operator () (Targ1 p) = 0;
};

template <class Tret>
class goFunctorBase0 : public goFunctorBase <Tret, void, void, void, void ,void, void, void, void ,void, void>
{
    public:
        goFunctorBase0 ()
            : goFunctorBase <Tret, void, void, void, void ,void, void, void, void ,void, void> ()
        {
        };
        virtual ~goFunctorBase0 ()
        {
        };

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () () = 0;
        virtual Tret operator () () const = 0;
};

template <class Tret, class Targ1>
class goFunctorBase1 : public goFunctorBase <Tret, Targ1, void, void, void ,void, void, void, void ,void, void>
{
    public:
        goFunctorBase1 ()
            : goFunctorBase <Tret, Targ1, void, void, void ,void, void, void, void ,void, void> ()
        {
        };
        virtual ~goFunctorBase1 ()
        {
        };

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ1 p) = 0;
        virtual Tret operator () (Targ1 p) const = 0;
};

template <class Tret, class Targ1, class Targ2>
class goFunctorBase2 : public goFunctorBase <Tret, Targ1, Targ2, void, void ,void, void, void, void ,void, void>
{
    public:
        goFunctorBase2 ()
            : goFunctorBase <Tret, Targ1, Targ2, void, void ,void, void, void, void ,void, void> ()
        {
        };
        virtual ~goFunctorBase2 ()
        {
        };

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ1 p, Targ2 p2) = 0;
        virtual Tret operator () (Targ1 p, Targ2 p2) const = 0;
};

template <class Tret>
class goFunction0 : public goFunctorBase0<Tret>
{
    public:
        typedef Tret (*function_t)();

    public:
        goFunction0 (function_t function)
            : goFunctorBase0<Tret>(), myFunction (function)
        {
        }
        virtual ~goFunction0 () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () ()
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                return (myFunction)();
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        }
        virtual Tret operator () () const
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                return (myFunction)();
            }
        }

    private:
        function_t myFunction;
};

template<> class goFunction0<void> : public goFunctorBase0<void>
{
    public:
        typedef void (*function_t)();

    public:
        goFunction0 (function_t function)
            : goFunctorBase0<void>(), myFunction (function)
        {
        }
        virtual ~goFunction0 () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () ()
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                (myFunction)();
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        }
        virtual void operator () () const
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                (myFunction)();
            }
        }

    private:
        function_t myFunction;
};

template <class Tret, class Tclass>
class goFunctor0 : public goFunctorBase0<Tret>
{
    public:
        typedef Tret(Tclass::*function_t)();

    public:
        goFunctor0 (Tclass* object, function_t function)
            : goFunctorBase0<Tret>(), myObject (object), myFunction (function)
        {
        };
        virtual ~goFunctor0 () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () ()
        {
            // printf ("Functor called.\n");
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)();
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        };

        virtual Tret operator () () const
        {
            // printf ("Functor called.\n");
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)();
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        }
    private:
        Tclass* myObject;
        function_t myFunction;
};

/** @brief Implementation template for
 * functions with 1 argument.
 */
template <class Tret, class Targ1>
class goFunction1 : public goFunctorBase1<Tret, Targ1>
{
    public:
        typedef Tret(*function_t)(Targ1);

    public:
        goFunction1 (function_t function)
            : goFunctorBase1<Tret, Targ1>(), myFunction (function)
        {
        };
        virtual ~goFunction1 () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ1 p)
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                return (myFunction)(p);
            }
        };
        virtual Tret operator () (Targ1 p) const
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                return (myFunction)(p);
            }
        };

    private:
        function_t myFunction;
};

template <class Targ1>
class goFunction1<void, Targ1> : public goFunctorBase1<void, Targ1>
{
    public:
        typedef void (*function_t)(Targ1);

    public:
        goFunction1 (function_t function)
            : goFunctorBase1<void, Targ1>(), myFunction (function)
        {
        }
        virtual ~goFunction1 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ1 p)
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                (myFunction)(p);
            }
        }
        virtual void operator () (Targ1 p) const
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                (myFunction)(p);
            }
        }

    private:
        function_t myFunction;
};

/** 
 * @brief Implementation template for
 * functors with 1 argument.
 */
template <class Tret, class Tclass, class Targ1>
class goFunctor1 : public goFunctorBase1<Tret, Targ1>
{
    public:
        typedef Tret(Tclass::*function_t)(Targ1);

    public:
        goFunctor1 (Tclass* object, function_t function)
            : goFunctorBase1<Tret, Targ1>(), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor1 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ1 p)
        {
            // printf ("Functor called.\n");
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)(p);
            }
            else
            {
                Tret dummy;
                return dummy;
            }
        };
        virtual Tret operator () (Targ1 p) const
        {
            // printf ("Functor called.\n");
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)(p);
            }
            else
            {
                Tret dummy;
                return dummy;
            }
        };

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** @brief Implementation template for
 * functors representing simple functions with 2 arguments.
 */
template <class Tret, class Targ1, class Targ2>
class goFunction2 : public goFunctorBase2<Tret, Targ1, Targ2>
{
    public:
        typedef Tret(*function_t)(Targ1, Targ2);

    public:
        goFunction2 (function_t function)
            : goFunctorBase2<Tret, Targ1, Targ2>(), myFunction (function)
        {
        };
        virtual ~goFunction2 () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ1 p, Targ2 p2)
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                return (myFunction)(p, p2);
            }
        };
        virtual Tret operator () (Targ1 p, Targ2 p2) const
        {
            // printf ("Functor called.\n");
            if (myFunction)
            {
                return (myFunction)(p, p2);
            }
        };

    private:
        function_t myFunction;
};
/** 
 * @brief Implementation template for
 * functors with 2 arguments.
 */
template <class Tret, class Tclass, class Targ1, class Targ2>
class goFunctor2 : public goFunctorBase2<Tret, Targ1, Targ2>
{
    public:
        typedef Tret(Tclass::*function_t)(Targ1, Targ2);

    public:
        goFunctor2 (Tclass* object, function_t function)
            : goFunctorBase2<Tret, Targ1, Targ2>(), myObject (object), myFunction (function)
        {
        };
        virtual ~goFunctor2 () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ1 p, Targ2 p2)
        {
            // printf ("Functor called.\n");
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)(p, p2);
            }
        };
        virtual Tret operator () (Targ1 p, Targ2 p2) const
        {
            // printf ("Functor called.\n");
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)(p, p2);
            }
        };

    private:
        Tclass* myObject;
        function_t myFunction;
};

#endif

#if 0
/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 * @todo Fix the goList issue. golist.hpp must be included at the
 * end of one source file that uses this class.
 */
template <class Tret>
class goCaller0
{
    public:
        typedef goList< goAutoPtr<goFunctorBase0<Tret> > > FunctorList;

    public:
        goCaller0 ()
            : fList () {};
        virtual ~goCaller0 () {fList.erase ();};

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        //= It may be a good idea to introduce a managed pointer class.
        void connect (goAutoPtr<goFunctorBase0<Tret> > f)
        {
            this->fList.append (f);
        };

        void disconnect (goAutoPtr<goFunctorBase0<Tret> > f)
        {
            typename FunctorList::Element* e = fList.find (f);
            if (e)
                fList.remove (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual Tret operator () ()
        {
            typename goList< goAutoPtr<goFunctorBase0<Tret> > >::Element* el = this->fList.getFrontElement ();
            Tret ret;
            while (el)
            {
                ret = (*el->elem)();
                el = el->next;
            }
            return ret;
        };

    private:
        FunctorList fList;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ1>
class goCaller1
{
    public:
        typedef goList< goAutoPtr<goFunctorBase1<Tret,Targ1> > > FunctorList;

    public:
        goCaller1 ()
            : fList () {};
        virtual ~goCaller1 () {fList.erase ();};

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        //= It may be a good idea to introduce a managed pointer class.
        void connect (goAutoPtr<goFunctorBase1<Tret, Targ1> > f)
        {
            this->fList.append (f);
        }

        void disconnect (goAutoPtr<goFunctorBase1<Tret, Targ1> > f)
        {
            typename FunctorList::Element* e = fList.find (f);
            if (e)
                fList.remove (e);
        }

        void clear ()
        {
            fList.clear ();
        }


        virtual Tret operator () (Targ1 a1)
        {
            typename goList< goAutoPtr<goFunctorBase1<Tret,Targ1> > >::Element* el = this->fList.getFrontElement ();
            Tret ret;
            while (el)
            {
                ret = (*el->elem)(a1);
                el = el->next;
            }
            return ret;
        }

    private:
        FunctorList fList;
};


template <class Tclass, class Tret>
goAutoPtr<goFunctorBase0<Tret> >
goMemberFunction (Tclass* c, typename goFunctor0<Tret, Tclass>::function_t f)
{
    return goAutoPtr<goFunctorBase0<Tret> > (static_cast<goFunctorBase0<Tret>*> (new goFunctor0<Tret, Tclass> (c, f)));
}

template <class Tret>
goAutoPtr<goFunctorBase0<Tret> >
goFunction (typename goFunction0<Tret>::function_t f)
{
    return goAutoPtr<goFunctorBase0<Tret> > (static_cast<goFunctorBase0<Tret>*> (new goFunction0<Tret> (f)));
}

template <class Tret, class Targ1>
goAutoPtr<goFunctorBase1<Tret, Targ1> >
goFunction (typename goFunction1<Tret, Targ1>::function_t f)
{
    return goAutoPtr<goFunctorBase1<Tret, Targ1> > (static_cast<goFunctorBase1<Tret, Targ1>*> (new goFunction1<Tret, Targ1> (f)));
}

template <class Tret, class Targ1, class Targ2>
goAutoPtr<goFunctorBase2<Tret, Targ1, Targ2> >
goFunction (typename goFunction2<Tret, Targ1, Targ2>::function_t f)
{
    return goAutoPtr<goFunctorBase2<Tret, Targ1, Targ2> > (static_cast<goFunctorBase2<Tret, Targ1, Targ2>*> (new goFunction2<Tret, Targ1, Targ2> (f)));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase1 object.
 */
template <class Tclass, class Tret, class Targ1>
goAutoPtr<goFunctorBase1<Tret, Targ1> >
goMemberFunction (Tclass* c, typename goFunctor1<Tret, Tclass, Targ1>::function_t f)
{
    return goAutoPtr<goFunctorBase1<Tret, Targ1> > (static_cast<goFunctorBase1<Tret, Targ1>*> (new goFunctor1<Tret, Tclass, Targ1> (c, f)));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase2 object.
 */
template <class Tclass, class Tret, class Targ1, class Targ2>
goAutoPtr<goFunctorBase2<Tret, Targ1, Targ2> >
goMemberFunction (Tclass* c, typename goFunctor2<Tret, Tclass, Targ1, Targ2>::function_t f)
{
    return goAutoPtr<goFunctorBase2<Tret, Targ1, Targ2> > (static_cast<goFunctorBase2<Tret, Targ1, Targ2>*> (new goFunctor2<Tret, Tclass, Targ1, Targ2> (c, f)));
}

#endif

#endif
