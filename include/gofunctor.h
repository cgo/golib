
//= Automatically created by mkfunctors.py.
//= Part of golib, (C) copyright 2009 by Christian Gosch

/**
 * \defgroup functors Functors
 *
 * \par Summary
 * This is a collection of templates which provides more or less flexible
 * functors which can represent either c-style functions or member functions
 * in a transparent-ish way. This code is generated automatically by a Python script
 * which is part of golib by Christian Gosch.<br>
 * There are also \a goCaller* objects which collect a number of functor objects
 * and can be used to broadcast a function call to all its functors.
 *
 * \par Usage
 * You can either create one of the \a goFunction* or \a goFunctor* objects by yourself,
 * or use the convenience functions \a goFunction() and \a goMemberFunction() to create
 * new functor objects.
 * 
 * \note The \a goFunction() and \a goMemberFunction() functions as well as the \a goCaller*
 * classes can either be created (by the source code generating script) with or without
 * the support for automatic pointers (from golib). It is recommended to use automatic pointers,
 * and \a goCaller* type objects have not been tested without automatic pointers.
 */

#ifndef GOFUNCTOR_H
#define GOFUNCTOR_H

#include <goautoptr.h>
#include <list>
#include <algorithm>
#include <assert.h>

  /**
   * \addtogroup functors
   * @{
   */

template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9, class Targ10>
class goFunctorBase
{
    public:
        goFunctorBase ()
        {        
        }
        virtual ~goFunctorBase ()
        {
        }
};

/** 
* @brief Base for functor objects with 0 arguments.
*/
template <class Tret>
class goFunctorBase0 : public goFunctorBase <Tret, void, void, void, void, void, void, void, void, void, void, void> 
{
    public:
        goFunctorBase0 ()
            : goFunctorBase <Tret, void, void, void, void, void, void, void, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase0 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () () = 0;
        virtual Tret operator () () const = 0;

        virtual goFunctorBase0 < Tret > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 1 arguments.
*/
template <class Tret, class Targ0>
class goFunctorBase1 : public goFunctorBase <Tret, Targ0, void, void, void, void, void, void, void, void, void, void> 
{
    public:
        goFunctorBase1 ()
            : goFunctorBase <Tret, Targ0, void, void, void, void, void, void, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase1 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0) = 0;
        virtual Tret operator () (Targ0 arg0) const = 0;

        virtual goFunctorBase1 < Tret, Targ0 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 2 arguments.
*/
template <class Tret, class Targ0, class Targ1>
class goFunctorBase2 : public goFunctorBase <Tret, Targ0, Targ1, void, void, void, void, void, void, void, void, void> 
{
    public:
        goFunctorBase2 ()
            : goFunctorBase <Tret, Targ0, Targ1, void, void, void, void, void, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase2 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1) const = 0;

        virtual goFunctorBase2 < Tret, Targ0, Targ1 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 3 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2>
class goFunctorBase3 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, void, void, void, void, void, void, void, void> 
{
    public:
        goFunctorBase3 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, void, void, void, void, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase3 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2) const = 0;

        virtual goFunctorBase3 < Tret, Targ0, Targ1, Targ2 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 4 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3>
class goFunctorBase4 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, void, void, void, void, void, void, void> 
{
    public:
        goFunctorBase4 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, void, void, void, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase4 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3) const = 0;

        virtual goFunctorBase4 < Tret, Targ0, Targ1, Targ2, Targ3 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 5 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
class goFunctorBase5 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, void, void, void, void, void, void> 
{
    public:
        goFunctorBase5 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, void, void, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase5 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4) const = 0;

        virtual goFunctorBase5 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 6 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
class goFunctorBase6 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, void, void, void, void, void> 
{
    public:
        goFunctorBase6 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, void, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase6 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5) const = 0;

        virtual goFunctorBase6 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 7 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
class goFunctorBase7 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, void, void, void, void> 
{
    public:
        goFunctorBase7 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, void, void, void, void> ()
        {
        }
        virtual ~goFunctorBase7 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6) const = 0;

        virtual goFunctorBase7 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 8 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
class goFunctorBase8 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, void, void, void> 
{
    public:
        goFunctorBase8 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, void, void, void> ()
        {
        }
        virtual ~goFunctorBase8 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7) const = 0;

        virtual goFunctorBase8 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 9 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
class goFunctorBase9 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, void, void> 
{
    public:
        goFunctorBase9 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, void, void> ()
        {
        }
        virtual ~goFunctorBase9 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8) const = 0;

        virtual goFunctorBase9 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > * createCopy () const = 0;
};

/** 
* @brief Base for functor objects with 10 arguments.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
class goFunctorBase10 : public goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9, void> 
{
    public:
        goFunctorBase10 ()
            : goFunctorBase <Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9, void> ()
        {
        }
        virtual ~goFunctorBase10 ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9) = 0;
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9) const = 0;

        virtual goFunctorBase10 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > * createCopy () const = 0;
};

/** 
* @brief Function representation (not member function) for functions with 0 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret>
class goFunction0 : public goFunctorBase0<Tret>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)();

    public:
        goFunction0 (function_t function)
            : goFunctorBase0<Tret> (), myFunction (function)
        {
        }
        virtual ~goFunction0 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () ()
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)();
            }
        }
        virtual Tret operator () () const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)();
            }
        }

        virtual goFunctorBase0 < Tret > * createCopy () const
        {
          return new goFunction0 < Tret > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <>
class goFunction0<void> : public goFunctorBase0<void>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)();

    public:
        goFunction0 (function_t function)
            : goFunctorBase0<void> (), myFunction (function)
        {
        }
        virtual ~goFunction0 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () ()
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)();
            }
        }
        virtual void operator () () const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)();
            }
        }

        virtual goFunctorBase0 < void > * createCopy () const
        {
          return new goFunction0 < void > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 0 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass>
class goFunctor0 : public goFunctorBase0<Tret>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)();

    public:
        goFunctor0 (Tclass* object, function_t function)
            : goFunctorBase0<Tret> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor0 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () ()
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)();
            }
        }
        virtual Tret operator () () const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)();
            }
        }

        virtual goFunctorBase0 < Tret > * createCopy () const
        {
          return new goFunctor0 < Tret, Tclass > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass>
class goFunctor0<void, Tclass> : public goFunctorBase0<void>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)();

    public:
        goFunctor0 (Tclass* object, function_t function)
            : goFunctorBase0<void> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor0 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () ()
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)();
            }
        }
        virtual void operator () () const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)();
            }
        }

        virtual goFunctorBase0 < void > * createCopy () const
        {
          return new goFunctor0 < void, Tclass > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret>
class goCaller0
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase0<Tret> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller0 ()
            : fList () {}
        virtual ~goCaller0 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () ()
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)();
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase0 object.
 */
template <class Tret>
goAutoPtr<  goFunctorBase0<Tret >  >
goFunction (Tret (*f)())
{
    return goAutoPtr<  goFunctorBase0<Tret>  > (new goFunction0<Tret> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase0 object.
 */
template <class Tret, class Tclass>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase0<Tret >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)())
{
    return goAutoPtr<  goFunctorBase0<Tret>  > (static_cast<goFunctorBase0<Tret>*> (new goFunctor0<Tret, Tclass> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 1 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0>
class goFunction1 : public goFunctorBase1<Tret, Targ0>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0);

    public:
        goFunction1 (function_t function)
            : goFunctorBase1<Tret, Targ0> (), myFunction (function)
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
        virtual Tret operator () (Targ0 arg0)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0);
            }
        }
        virtual Tret operator () (Targ0 arg0) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0);
            }
        }

        virtual goFunctorBase1 < Tret, Targ0 > * createCopy () const
        {
          return new goFunction1 < Tret, Targ0 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0>
class goFunction1<void, Targ0> : public goFunctorBase1<void, Targ0>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0);

    public:
        goFunction1 (function_t function)
            : goFunctorBase1<void, Targ0> (), myFunction (function)
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
        virtual void operator () (Targ0 arg0)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0);
            }
        }
        virtual void operator () (Targ0 arg0) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0);
            }
        }

        virtual goFunctorBase1 < void, Targ0 > * createCopy () const
        {
          return new goFunction1 < void, Targ0 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 1 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0>
class goFunctor1 : public goFunctorBase1<Tret, Targ0>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0);

    public:
        goFunctor1 (Tclass* object, function_t function)
            : goFunctorBase1<Tret, Targ0> (), myObject (object), myFunction (function)
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
        virtual Tret operator () (Targ0 arg0)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0);
            }
        }
        virtual Tret operator () (Targ0 arg0) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0);
            }
        }

        virtual goFunctorBase1 < Tret, Targ0 > * createCopy () const
        {
          return new goFunctor1 < Tret, Tclass, Targ0 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0>
class goFunctor1<void, Tclass, Targ0> : public goFunctorBase1<void, Targ0>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0);

    public:
        goFunctor1 (Tclass* object, function_t function)
            : goFunctorBase1<void, Targ0> (), myObject (object), myFunction (function)
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
        virtual void operator () (Targ0 arg0)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0);
            }
        }
        virtual void operator () (Targ0 arg0) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0);
            }
        }

        virtual goFunctorBase1 < void, Targ0 > * createCopy () const
        {
          return new goFunctor1 < void, Tclass, Targ0 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0>
class goCaller1
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase1<Tret, Targ0> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller1 ()
            : fList () {}
        virtual ~goCaller1 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase1 object.
 */
template <class Tret, class Targ0>
goAutoPtr<  goFunctorBase1<Tret, Targ0 >  >
goFunction (Tret (*f)(Targ0 arg0))
{
    return goAutoPtr<  goFunctorBase1<Tret, Targ0>  > (new goFunction1<Tret, Targ0> (f));
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
template <class Tret, class Tclass, class Targ0>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase1<Tret, Targ0 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0))
{
    return goAutoPtr<  goFunctorBase1<Tret, Targ0>  > (static_cast<goFunctorBase1<Tret, Targ0>*> (new goFunctor1<Tret, Tclass, Targ0> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 2 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1>
class goFunction2 : public goFunctorBase2<Tret, Targ0, Targ1>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1);

    public:
        goFunction2 (function_t function)
            : goFunctorBase2<Tret, Targ0, Targ1> (), myFunction (function)
        {
        }
        virtual ~goFunction2 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1);
            }
        }

        virtual goFunctorBase2 < Tret, Targ0, Targ1 > * createCopy () const
        {
          return new goFunction2 < Tret, Targ0, Targ1 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1>
class goFunction2<void, Targ0, Targ1> : public goFunctorBase2<void, Targ0, Targ1>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1);

    public:
        goFunction2 (function_t function)
            : goFunctorBase2<void, Targ0, Targ1> (), myFunction (function)
        {
        }
        virtual ~goFunction2 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1);
            }
        }

        virtual goFunctorBase2 < void, Targ0, Targ1 > * createCopy () const
        {
          return new goFunction2 < void, Targ0, Targ1 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 2 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1>
class goFunctor2 : public goFunctorBase2<Tret, Targ0, Targ1>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1);

    public:
        goFunctor2 (Tclass* object, function_t function)
            : goFunctorBase2<Tret, Targ0, Targ1> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor2 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1);
            }
        }

        virtual goFunctorBase2 < Tret, Targ0, Targ1 > * createCopy () const
        {
          return new goFunctor2 < Tret, Tclass, Targ0, Targ1 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1>
class goFunctor2<void, Tclass, Targ0, Targ1> : public goFunctorBase2<void, Targ0, Targ1>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1);

    public:
        goFunctor2 (Tclass* object, function_t function)
            : goFunctorBase2<void, Targ0, Targ1> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor2 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1);
            }
        }

        virtual goFunctorBase2 < void, Targ0, Targ1 > * createCopy () const
        {
          return new goFunctor2 < void, Tclass, Targ0, Targ1 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1>
class goCaller2
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase2<Tret, Targ0, Targ1> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller2 ()
            : fList () {}
        virtual ~goCaller2 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase2 object.
 */
template <class Tret, class Targ0, class Targ1>
goAutoPtr<  goFunctorBase2<Tret, Targ0, Targ1 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1))
{
    return goAutoPtr<  goFunctorBase2<Tret, Targ0, Targ1>  > (new goFunction2<Tret, Targ0, Targ1> (f));
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
template <class Tret, class Tclass, class Targ0, class Targ1>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase2<Tret, Targ0, Targ1 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1))
{
    return goAutoPtr<  goFunctorBase2<Tret, Targ0, Targ1>  > (static_cast<goFunctorBase2<Tret, Targ0, Targ1>*> (new goFunctor2<Tret, Tclass, Targ0, Targ1> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 3 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2>
class goFunction3 : public goFunctorBase3<Tret, Targ0, Targ1, Targ2>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2);

    public:
        goFunction3 (function_t function)
            : goFunctorBase3<Tret, Targ0, Targ1, Targ2> (), myFunction (function)
        {
        }
        virtual ~goFunction3 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2);
            }
        }

        virtual goFunctorBase3 < Tret, Targ0, Targ1, Targ2 > * createCopy () const
        {
          return new goFunction3 < Tret, Targ0, Targ1, Targ2 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2>
class goFunction3<void, Targ0, Targ1, Targ2> : public goFunctorBase3<void, Targ0, Targ1, Targ2>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2);

    public:
        goFunction3 (function_t function)
            : goFunctorBase3<void, Targ0, Targ1, Targ2> (), myFunction (function)
        {
        }
        virtual ~goFunction3 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2);
            }
        }

        virtual goFunctorBase3 < void, Targ0, Targ1, Targ2 > * createCopy () const
        {
          return new goFunction3 < void, Targ0, Targ1, Targ2 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 3 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2>
class goFunctor3 : public goFunctorBase3<Tret, Targ0, Targ1, Targ2>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2);

    public:
        goFunctor3 (Tclass* object, function_t function)
            : goFunctorBase3<Tret, Targ0, Targ1, Targ2> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor3 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2);
            }
        }

        virtual goFunctorBase3 < Tret, Targ0, Targ1, Targ2 > * createCopy () const
        {
          return new goFunctor3 < Tret, Tclass, Targ0, Targ1, Targ2 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2>
class goFunctor3<void, Tclass, Targ0, Targ1, Targ2> : public goFunctorBase3<void, Targ0, Targ1, Targ2>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2);

    public:
        goFunctor3 (Tclass* object, function_t function)
            : goFunctorBase3<void, Targ0, Targ1, Targ2> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor3 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2);
            }
        }

        virtual goFunctorBase3 < void, Targ0, Targ1, Targ2 > * createCopy () const
        {
          return new goFunctor3 < void, Tclass, Targ0, Targ1, Targ2 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2>
class goCaller3
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase3<Tret, Targ0, Targ1, Targ2> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller3 ()
            : fList () {}
        virtual ~goCaller3 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase3 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2>
goAutoPtr<  goFunctorBase3<Tret, Targ0, Targ1, Targ2 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2))
{
    return goAutoPtr<  goFunctorBase3<Tret, Targ0, Targ1, Targ2>  > (new goFunction3<Tret, Targ0, Targ1, Targ2> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase3 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase3<Tret, Targ0, Targ1, Targ2 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2))
{
    return goAutoPtr<  goFunctorBase3<Tret, Targ0, Targ1, Targ2>  > (static_cast<goFunctorBase3<Tret, Targ0, Targ1, Targ2>*> (new goFunctor3<Tret, Tclass, Targ0, Targ1, Targ2> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 4 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3>
class goFunction4 : public goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3);

    public:
        goFunction4 (function_t function)
            : goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3> (), myFunction (function)
        {
        }
        virtual ~goFunction4 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3);
            }
        }

        virtual goFunctorBase4 < Tret, Targ0, Targ1, Targ2, Targ3 > * createCopy () const
        {
          return new goFunction4 < Tret, Targ0, Targ1, Targ2, Targ3 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2, class Targ3>
class goFunction4<void, Targ0, Targ1, Targ2, Targ3> : public goFunctorBase4<void, Targ0, Targ1, Targ2, Targ3>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3);

    public:
        goFunction4 (function_t function)
            : goFunctorBase4<void, Targ0, Targ1, Targ2, Targ3> (), myFunction (function)
        {
        }
        virtual ~goFunction4 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3);
            }
        }

        virtual goFunctorBase4 < void, Targ0, Targ1, Targ2, Targ3 > * createCopy () const
        {
          return new goFunction4 < void, Targ0, Targ1, Targ2, Targ3 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 4 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3>
class goFunctor4 : public goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3);

    public:
        goFunctor4 (Tclass* object, function_t function)
            : goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor4 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3);
            }
        }

        virtual goFunctorBase4 < Tret, Targ0, Targ1, Targ2, Targ3 > * createCopy () const
        {
          return new goFunctor4 < Tret, Tclass, Targ0, Targ1, Targ2, Targ3 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2, class Targ3>
class goFunctor4<void, Tclass, Targ0, Targ1, Targ2, Targ3> : public goFunctorBase4<void, Targ0, Targ1, Targ2, Targ3>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3);

    public:
        goFunctor4 (Tclass* object, function_t function)
            : goFunctorBase4<void, Targ0, Targ1, Targ2, Targ3> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor4 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3);
            }
        }

        virtual goFunctorBase4 < void, Targ0, Targ1, Targ2, Targ3 > * createCopy () const
        {
          return new goFunctor4 < void, Tclass, Targ0, Targ1, Targ2, Targ3 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3>
class goCaller4
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller4 ()
            : fList () {}
        virtual ~goCaller4 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2, arg3);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase4 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3>
goAutoPtr<  goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3))
{
    return goAutoPtr<  goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3>  > (new goFunction4<Tret, Targ0, Targ1, Targ2, Targ3> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase4 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3))
{
    return goAutoPtr<  goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3>  > (static_cast<goFunctorBase4<Tret, Targ0, Targ1, Targ2, Targ3>*> (new goFunctor4<Tret, Tclass, Targ0, Targ1, Targ2, Targ3> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 5 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
class goFunction5 : public goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4);

    public:
        goFunction5 (function_t function)
            : goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4> (), myFunction (function)
        {
        }
        virtual ~goFunction5 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }

        virtual goFunctorBase5 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4 > * createCopy () const
        {
          return new goFunction5 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
class goFunction5<void, Targ0, Targ1, Targ2, Targ3, Targ4> : public goFunctorBase5<void, Targ0, Targ1, Targ2, Targ3, Targ4>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4);

    public:
        goFunction5 (function_t function)
            : goFunctorBase5<void, Targ0, Targ1, Targ2, Targ3, Targ4> (), myFunction (function)
        {
        }
        virtual ~goFunction5 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }

        virtual goFunctorBase5 < void, Targ0, Targ1, Targ2, Targ3, Targ4 > * createCopy () const
        {
          return new goFunction5 < void, Targ0, Targ1, Targ2, Targ3, Targ4 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 5 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
class goFunctor5 : public goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4);

    public:
        goFunctor5 (Tclass* object, function_t function)
            : goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor5 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }

        virtual goFunctorBase5 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4 > * createCopy () const
        {
          return new goFunctor5 < Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
class goFunctor5<void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4> : public goFunctorBase5<void, Targ0, Targ1, Targ2, Targ3, Targ4>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4);

    public:
        goFunctor5 (Tclass* object, function_t function)
            : goFunctorBase5<void, Targ0, Targ1, Targ2, Targ3, Targ4> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor5 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4);
            }
        }

        virtual goFunctorBase5 < void, Targ0, Targ1, Targ2, Targ3, Targ4 > * createCopy () const
        {
          return new goFunctor5 < void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
class goCaller5
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller5 ()
            : fList () {}
        virtual ~goCaller5 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2, arg3, arg4);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase5 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
goAutoPtr<  goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4))
{
    return goAutoPtr<  goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4>  > (new goFunction5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase5 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4))
{
    return goAutoPtr<  goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4>  > (static_cast<goFunctorBase5<Tret, Targ0, Targ1, Targ2, Targ3, Targ4>*> (new goFunctor5<Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 6 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
class goFunction6 : public goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5);

    public:
        goFunction6 (function_t function)
            : goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> (), myFunction (function)
        {
        }
        virtual ~goFunction6 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }

        virtual goFunctorBase6 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > * createCopy () const
        {
          return new goFunction6 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
class goFunction6<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> : public goFunctorBase6<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5);

    public:
        goFunction6 (function_t function)
            : goFunctorBase6<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> (), myFunction (function)
        {
        }
        virtual ~goFunction6 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }

        virtual goFunctorBase6 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > * createCopy () const
        {
          return new goFunction6 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 6 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
class goFunctor6 : public goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5);

    public:
        goFunctor6 (Tclass* object, function_t function)
            : goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor6 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }

        virtual goFunctorBase6 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > * createCopy () const
        {
          return new goFunctor6 < Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
class goFunctor6<void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> : public goFunctorBase6<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5);

    public:
        goFunctor6 (Tclass* object, function_t function)
            : goFunctorBase6<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor6 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }

        virtual goFunctorBase6 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > * createCopy () const
        {
          return new goFunctor6 < void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
class goCaller6
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller6 ()
            : fList () {}
        virtual ~goCaller6 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2, arg3, arg4, arg5);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase6 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
goAutoPtr<  goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5))
{
    return goAutoPtr<  goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5>  > (new goFunction6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase6 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5))
{
    return goAutoPtr<  goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5>  > (static_cast<goFunctorBase6<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5>*> (new goFunctor6<Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 7 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
class goFunction7 : public goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6);

    public:
        goFunction7 (function_t function)
            : goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> (), myFunction (function)
        {
        }
        virtual ~goFunction7 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }

        virtual goFunctorBase7 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > * createCopy () const
        {
          return new goFunction7 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
class goFunction7<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> : public goFunctorBase7<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6);

    public:
        goFunction7 (function_t function)
            : goFunctorBase7<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> (), myFunction (function)
        {
        }
        virtual ~goFunction7 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }

        virtual goFunctorBase7 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > * createCopy () const
        {
          return new goFunction7 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 7 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
class goFunctor7 : public goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6);

    public:
        goFunctor7 (Tclass* object, function_t function)
            : goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor7 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }

        virtual goFunctorBase7 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > * createCopy () const
        {
          return new goFunctor7 < Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
class goFunctor7<void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> : public goFunctorBase7<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6);

    public:
        goFunctor7 (Tclass* object, function_t function)
            : goFunctorBase7<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor7 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }

        virtual goFunctorBase7 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > * createCopy () const
        {
          return new goFunctor7 < void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
class goCaller7
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller7 ()
            : fList () {}
        virtual ~goCaller7 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase7 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
goAutoPtr<  goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6))
{
    return goAutoPtr<  goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6>  > (new goFunction7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase7 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6))
{
    return goAutoPtr<  goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6>  > (static_cast<goFunctorBase7<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6>*> (new goFunctor7<Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 8 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
class goFunction8 : public goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7);

    public:
        goFunction8 (function_t function)
            : goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> (), myFunction (function)
        {
        }
        virtual ~goFunction8 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }

        virtual goFunctorBase8 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > * createCopy () const
        {
          return new goFunction8 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
class goFunction8<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> : public goFunctorBase8<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7);

    public:
        goFunction8 (function_t function)
            : goFunctorBase8<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> (), myFunction (function)
        {
        }
        virtual ~goFunction8 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }

        virtual goFunctorBase8 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > * createCopy () const
        {
          return new goFunction8 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 8 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
class goFunctor8 : public goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7);

    public:
        goFunctor8 (Tclass* object, function_t function)
            : goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor8 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }

        virtual goFunctorBase8 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > * createCopy () const
        {
          return new goFunctor8 < Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
class goFunctor8<void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> : public goFunctorBase8<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7);

    public:
        goFunctor8 (Tclass* object, function_t function)
            : goFunctorBase8<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor8 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }

        virtual goFunctorBase8 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > * createCopy () const
        {
          return new goFunctor8 < void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
class goCaller8
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller8 ()
            : fList () {}
        virtual ~goCaller8 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase8 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
goAutoPtr<  goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7))
{
    return goAutoPtr<  goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7>  > (new goFunction8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase8 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7))
{
    return goAutoPtr<  goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7>  > (static_cast<goFunctorBase8<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7>*> (new goFunctor8<Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 9 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
class goFunction9 : public goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8);

    public:
        goFunction9 (function_t function)
            : goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> (), myFunction (function)
        {
        }
        virtual ~goFunction9 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }

        virtual goFunctorBase9 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > * createCopy () const
        {
          return new goFunction9 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
class goFunction9<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> : public goFunctorBase9<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8);

    public:
        goFunction9 (function_t function)
            : goFunctorBase9<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> (), myFunction (function)
        {
        }
        virtual ~goFunction9 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }

        virtual goFunctorBase9 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > * createCopy () const
        {
          return new goFunction9 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 9 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
class goFunctor9 : public goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8);

    public:
        goFunctor9 (Tclass* object, function_t function)
            : goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor9 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }

        virtual goFunctorBase9 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > * createCopy () const
        {
          return new goFunctor9 < Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
class goFunctor9<void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> : public goFunctorBase9<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8);

    public:
        goFunctor9 (Tclass* object, function_t function)
            : goFunctorBase9<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor9 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }

        virtual goFunctorBase9 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > * createCopy () const
        {
          return new goFunctor9 < void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
class goCaller9
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller9 ()
            : fList () {}
        virtual ~goCaller9 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase9 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
goAutoPtr<  goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8))
{
    return goAutoPtr<  goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8>  > (new goFunction9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase9 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8))
{
    return goAutoPtr<  goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8>  > (static_cast<goFunctorBase9<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8>*> (new goFunctor9<Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8> (c, f)));
}

/** 
* @brief Function representation (not member function) for functions with 10 arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
class goFunction10 : public goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9);

    public:
        goFunction10 (function_t function)
            : goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> (), myFunction (function)
        {
        }
        virtual ~goFunction10 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9)
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }

        virtual goFunctorBase10 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > * createCopy () const
        {
          return new goFunction10 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
template <class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
class goFunction10<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> : public goFunctorBase10<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9);

    public:
        goFunction10 (function_t function)
            : goFunctorBase10<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> (), myFunction (function)
        {
        }
        virtual ~goFunction10 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9)
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }

        virtual goFunctorBase10 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > * createCopy () const
        {
          return new goFunction10 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with 10 arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
class goFunctor10 : public goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9);

    public:
        goFunctor10 (Tclass* object, function_t function)
            : goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor10 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }
        virtual Tret operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }

        virtual goFunctorBase10 < Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > * createCopy () const
        {
          return new goFunctor10 < Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
template <class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
class goFunctor10<void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> : public goFunctorBase10<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9);

    public:
        goFunctor10 (Tclass* object, function_t function)
            : goFunctorBase10<void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor10 () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9)
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }
        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }

        virtual goFunctorBase10 < void, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > * createCopy () const
        {
          return new goFunctor10 < void, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
class goCaller10
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> FunctorBase;
        typedef std::list< goAutoPtr<  FunctorBase  > > FunctorList;

    public:
        goCaller10 ()
            : fList () {}
        virtual ~goCaller10 () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<  FunctorBase  > f)
        {
            this->fList.push_back (f);
        }

        void disconnect (goAutoPtr<  FunctorBase  > f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9)
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            }
        }

    private:
        FunctorList fList;
};

/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a goFunctorBase10 object.
 */
template <class Tret, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
goAutoPtr<  goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 >  >
goFunction (Tret (*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9))
{
    return goAutoPtr<  goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9>  > (new goFunction10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase10 object.
 */
template <class Tret, class Tclass, class Targ0, class Targ1, class Targ2, class Targ3, class Targ4, class Targ5, class Targ6, class Targ7, class Targ8, class Targ9>
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<  goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9 >  >
goMemberFunction (Tclass* c, Tret (Tclass::*f)(Targ0 arg0, Targ1 arg1, Targ2 arg2, Targ3 arg3, Targ4 arg4, Targ5 arg5, Targ6 arg6, Targ7 arg7, Targ8 arg8, Targ9 arg9))
{
    return goAutoPtr<  goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9>  > (static_cast<goFunctorBase10<Tret, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9>*> (new goFunctor10<Tret, Tclass, Targ0, Targ1, Targ2, Targ3, Targ4, Targ5, Targ6, Targ7, Targ8, Targ9> (c, f)));
}


  /** @} */
#endif

