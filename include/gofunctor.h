#ifndef GOFUNCTOR_H
#define GOFUNCTOR_H

#include <goautoptr.h>
#include <golist.h>
#include <golog.h>

/**
 * @addtogroup misc
 * @{
 */
/** 
 * @brief Virtual base class for functors with 1 argument.
 */
template <class Tret, class Targ1>
class goFunctorBase1T
{
    public:
        goFunctorBase1T ()
        {
        };
        virtual ~goFunctorBase1T ()
        {
        };

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ1 p) = 0;
};

template <class Tret, class Targ1, class Targ2>
class goFunctorBase2T
{
    public:
        goFunctorBase2T ()
        {
        };
        virtual ~goFunctorBase2T ()
        {
        };

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ1 p, Targ2 p2) = 0;
};

/** 
 * @brief Implementation template for
 * functors with 1 argument.
 */
template <class Tret, class Tclass, class Targ1>
class goFunctor1T : public goFunctorBase1T<Tret, Targ1>
{
    public:
        typedef Tret(Tclass::*function_t)(Targ1);

    public:
        goFunctor1T (Tclass* object, function_t function)
            : goFunctorBase1T<Tret, Targ1>(), myObject (object), myFunction (function)
        {
        };
        virtual ~goFunctor1T () {};

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
        };

    private:
        Tclass* myObject;
        function_t myFunction;
};

template <class Tret, class Tclass, class Targ1, class Targ2>
class goFunctor2T : public goFunctorBase2T<Tret, Targ1, Targ2>
{
    public:
        typedef Tret(Tclass::*function_t)(Targ1, Targ2);

    public:
        goFunctor2T (Tclass* object, function_t function)
            : goFunctorBase2T<Tret, Targ1, Targ2>(), myObject (object), myFunction (function)
        {
        };
        virtual ~goFunctor2T () {};

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

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 * @todo Fix the goList issue. golist.hpp must be included at the
 * end of one source file that uses this class.
 */
template <class Tret, class Targ1>
class goCaller1
{
    public:
        goCaller1 ()
            : fList () {};
        virtual ~goCaller1 () {fList.erase ();};

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        //= It may be a good idea to introduce a managed pointer class.
        void connect (goAutoPtr<goFunctorBase1T<Tret, Targ1> > f)
        {
            this->fList.append (f);
        };

        virtual Tret operator () (Targ1 a1)
        {
            typename goList< goAutoPtr<goFunctorBase1T<Tret,Targ1> > >::Element* el = this->fList.getFrontElement ();
            while (el)
            {
                (*el->elem)(a1);
                el = el->next;
            }
        };

    private:
        goList< goAutoPtr<goFunctorBase1T<Tret,Targ1> > > fList;
};

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
goAutoPtr<goFunctorBase1T<Tret, Targ1> >
goMemberFunction (Tclass* c, typename goFunctor1T<Tret, Tclass, Targ1>::function_t f)
{
    return goAutoPtr<goFunctorBase1T<Tret, Targ1> > (static_cast<goFunctorBase1T<Tret, Targ1>*> (new goFunctor1T<Tret, Tclass, Targ1> (c, f)));
}

/** @} */
#endif
