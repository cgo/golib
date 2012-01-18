#!/usr/bin/python

import string, sys

"""
Helper program to create functor implementations up to a given max. number of arguments.
Versions for void and non-void are made, as well as matching helpers (goFunction() and goMemberFunction())
and goCaller classes.
Part of golib: See http://www.goschs.de
(C) Copyright 2009 by Christian Gosch
"""

FUNCTORBASE = "goFunctorBase"
FUNCTION    = "goFunction"
FUNCTOR     = "goFunctor"
CALLER      = "goCaller"
MEMBERFUNCTION = "goMemberFunction"
USE_AUTOPTR = True
AUTOPTR     = "goAutoPtr"

header = """
#ifndef GOFUNCTOR_H
#define GOFUNCTOR_H

#include <goautoptr.h>
#include <list>
#include <algorithm>
#include <assert.h>

  /**
   * \\addtogroup functors
   * @{
   */
"""

docs = """
/**
 * \defgroup functors Functors
 *
 * \par Summary
 * This is a collection of templates which provides more or less flexible
 * functors which can represent either c-style functions or member functions
 * in a transparent-ish way. This code is generated automatically by a Python script
 * which is part of golib by Christian Gosch.<br>
 * There are also \\a {caller}* objects which collect a number of functor objects
 * and can be used to broadcast a function call to all its functors.
 *
 * \par Usage
 * You can either create one of the \\a {function}* or \\a {functor}* objects by yourself,
 * or use the convenience functions \\a {function}() and \\a {memberfunction}() to create
 * new functor objects.
 * 
 * \\note The \\a {function}() and \\a {memberfunction}() functions as well as the \\a {caller}*
 * classes can either be created (by the source code generating script) with or without
 * the support for automatic pointers (from golib). It is recommended to use automatic pointers,
 * and \\a {caller}* type objects have not been tested without automatic pointers.
 */
""".format (function = FUNCTION, functor = FUNCTOR, memberfunction = MEMBERFUNCTION, caller = CALLER)

footer = """
  /** @} */
#endif
"""

def make_base (max_args):
    args_string = "class Tret"
    for i in xrange (max_args):
        args_string += ", class Targ" + str(i)
    return """
template <""" + args_string + """>
class {0}
{{
    public:
        {0} ()
        {{        
        }}
        virtual ~{0} ()
        {{
        }}
}};
""".format (FUNCTORBASE)

def make_functors (num_args, max_args, void_ret = False, base = False, make_callers = False, make_helper = False):

    template_base_string = """
/** 
* @brief Base for functor objects with ${postfix} arguments.
*/
${template_statement_functorbase}
class ${functorbase}${postfix} : public ${functorbase} <${template_args_base}> 
{
    public:
        ${functorbase}${postfix} ()
            : ${functorbase} <${template_args_base}> ()
        {
        }
        virtual ~${functorbase}${postfix} ()
        {
        }

        /** 
         * @brief Purely virtual function. In derived classes,
         * calls the function represented by this functor.
         * 
         * @return Any value.
         */
        virtual Tret operator () (${args_names}) = 0;
        virtual Tret operator () (${args_names}) const = 0;

        virtual ${functorbase}${postfix} < ${template_args_function} > * createCopy () const = 0;
};
"""
    template_string = """
/** 
* @brief Function representation (not member function) for functions with ${postfix} arguments.
* 
* The first template argument is the return type, the others are argument types.
*/
${template_statement_function}
class ${function}${postfix} : public ${functorbase}${postfix}<${template_args_function}>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (*function_t)(${args_names});

    public:
        ${function}${postfix} (function_t function)
            : ${functorbase}${postfix}<${template_args_function}> (), myFunction (function)
        {
        }
        virtual ~${function}${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (${args_names})
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(${names});
            }
        }
        virtual Tret operator () (${args_names}) const
        {
            assert (0 != myFunction);
            // if (myFunction)
            {
                return (myFunction)(${names});
            }
        }

        virtual ${functorbase}${postfix} < ${template_args_function} > * createCopy () const
        {
          return new ${function}${postfix} < ${template_args_function} > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Specialisation for void functions.
*/
${template_statement_function_void}
class ${function}${postfix}<${template_args_function_void}> : public ${functorbase}${postfix}<${template_args_function_void}>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (*function_t)(${args_names});

    public:
        ${function}${postfix} (function_t function)
            : ${functorbase}${postfix}<${template_args_function_void}> (), myFunction (function)
        {
        }
        virtual ~${function}${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (${args_names})
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(${names});
            }
        }
        virtual void operator () (${args_names}) const
        {
            assert (0 != myFunction);
            if (myFunction)
            {
                (myFunction)(${names});
            }
        }

        virtual ${functorbase}${postfix} < ${template_args_function_void} > * createCopy () const
        {
          return new ${function}${postfix} < ${template_args_function_void} > (*this);
        }

    private:
        function_t myFunction;
};

/** 
* @brief Member function representation for class members with ${postfix} arguments.
* 
* The first template argument is the return type, the second is the class type,
* the others are member function argument types.
*/
${template_statement_functor}
class ${functor}${postfix} : public ${functorbase}${postfix}<${template_args_function}>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef Tret (Tclass::*function_t)(${args_names});

    public:
        ${functor}${postfix} (Tclass* object, function_t function)
            : ${functorbase}${postfix}<${template_args_function}> (), myObject (object), myFunction (function)
        {
        }
        virtual ~${functor}${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (${args_names})
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(${names});
            }
        }
        virtual Tret operator () (${args_names}) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                return (myObject->*myFunction)(${names});
            }
        }

        virtual ${functorbase}${postfix} < ${template_args_function} > * createCopy () const
        {
          return new ${functor}${postfix} < ${template_args_functor} > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

/** 
* @brief Specialisation for void member functions.
*/
${template_statement_functor_void}
class ${functor}${postfix}<${template_args_functor_void}> : public ${functorbase}${postfix}<${template_args_function_void}>
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef void (Tclass::*function_t)(${args_names});

    public:
        ${functor}${postfix} (Tclass* object, function_t function)
            : ${functorbase}${postfix}<${template_args_function_void}> (), myObject (object), myFunction (function)
        {
        }
        virtual ~${functor}${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (${args_names})
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(${names});
            }
        }
        virtual void operator () (${args_names}) const
        {
            assert (myObject && myFunction); 
            // if (myObject && myFunction)
            {
                (myObject->*myFunction)(${names});
            }
        }

        virtual ${functorbase}${postfix} < ${template_args_function_void} > * createCopy () const
        {
          return new ${functor}${postfix} < ${template_args_functor_void} > (*this);
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};
"""

    caller_template = """
/** 
 * @brief Broadcasting caller class,
 * like signals in the "signal/slot" paradigm.
 */
${template_statement_functorbase}
class ${caller}${postfix}
{
    public:
        /** 
        * @brief Function type represented by this class.
        */
        typedef ${functorbase}${postfix}<${template_args_function}> FunctorBase;
        typedef std::list< ${autoptrstart} FunctorBase ${autoptrend} > FunctorList;

    public:
        ${caller}${postfix} ()
            : fList () {}
        virtual ~${caller}${postfix} () {fList.clear ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (${autoptrstart} FunctorBase ${autoptrend} f)
        {
            this->fList.push_back (f);
        }

        void disconnect (${autoptrstart} FunctorBase ${autoptrend} f)
        {
            typename FunctorList::iterator e = std::find (fList.begin(), fList.end(), f);
            if (e != fList.end ())
                fList.erase (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        virtual void operator () (${args_names})
        {
            typename FunctorList::iterator el = this->fList.begin ();
            for (el = this->fList.begin (); el != this->fList.end (); ++el)
            {
                (**el)(${names});
            }
        }

    private:
        FunctorList fList;
};
"""

    helper_template = """
/** 
 * @brief Create a function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return goAutoPtr to a ${functorbase}${postfix} object.
 */
${template_statement_function}
${autoptrstart} ${functorbase}${postfix}<${template_args_function} > ${autoptrend}
${function} (Tret (*f)(${args_names}))
{
    return ${autoptrstart} ${functorbase}${postfix}<${template_args_function}> ${autoptrend} (new ${function}${postfix}<${template_args_function}> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a ${functorbase}${postfix} object.
 */
${template_statement_functor}
// template <class Tclass, class Tret, class Targ1>
${autoptrstart} ${functorbase}${postfix}<${template_args_function} > ${autoptrend}
${memberfunction} (Tclass* c, Tret (Tclass::*f)(${args_names}))
{
    return ${autoptrstart} ${functorbase}${postfix}<${template_args_function}> ${autoptrend} (static_cast<${functorbase}${postfix}<${template_args_function}>*> (new ${functor}${postfix}<${template_args_functor}> (c, f)));
}
"""

    helper_template_no_autoptr = """
/** 
 * @brief Create a function functor object.
 * 
 * @param f  Function pointer to a function.
 * 
 * @return Pointer to a ${functorbase}${postfix} object pointer.
 */
${template_statement_function}
${functorbase}${postfix}<${template_args_function} >*
${function} (Tret (*f)(${args_names}))
{
    return new ${function}${postfix}<${template_args_function}> (f);
}

/** 
 * @brief Create a member function functor object.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return Pointer to a ${functorbase}${postfix} object.
 */
${template_statement_functor}
${functorbase}${postfix}<${template_args_function} >*
${memberfunction} (Tclass* c, Tret (Tclass::*f)(${args_names}))
{
    return static_cast<${functorbase}${postfix}<${template_args_function}>*> (new ${functor}${postfix}<${template_args_functor}> (c, f));
}
"""

    args_string = ""
    args_names_string = ""
    names_string = ""

    args_string2 = ""

    args_names = ""
    names = ""
    template_statement_functorbase = "template <class Tret"
    template_args_base = "Tret"

    for i in xrange (num_args):
        template_statement_functorbase += ", class Targ" + str(i)
        template_args_base += ", Targ" + str(i)
        if i == 0:
            args_names = "Targ0 arg0"
            names = "arg0"
        else:
            args_names += ", Targ" + str(i) + " arg" + str(i)
            names += ", arg" + str(i)
            
    for i in xrange (max_args - num_args):
        template_args_base += ", void"
    template_statement_functorbase += ">"
    
    template_statement_functor = "template <class Tret, class Tclass"
    template_args_functor = "Tret, Tclass"
    template_statement_functor_void = "template <class Tclass"
    template_args_functor_void = "void, Tclass"

    template_statement_function = "template <class Tret"
    template_statement_function_void = "template <"
    template_args_function_void = "void"
    template_args_function = "Tret"


    if num_args > 0:
        template_statement_function += ", class Targ0"
        template_statement_function_void += "class Targ0"
        template_statement_functor += ", class Targ0"
        template_statement_functor_void += ", class Targ0"
        template_args_functor += ", Targ0"
        template_args_functor_void += ", Targ0"
        template_args_function += ", Targ0"
        template_args_function_void += ", Targ0"
        for i in xrange (1, num_args):
            s = ", class Targ" + str(i)
            template_statement_function += s
            template_statement_function_void += s
            template_statement_functor += s
            template_statement_functor_void += s
            template_args_functor += ", Targ" + str(i)
            template_args_functor_void += ", Targ" + str(i)
            template_args_function += ", Targ" + str(i)
            template_args_function_void += ", Targ" + str(i)

    template_statement_function += ">"
    template_statement_function_void += ">"
    template_statement_functor += ">"
    template_statement_functor_void += ">"

    if (base):
        s = string.Template (template_base_string)
    else:
        s = string.Template (template_string)

    d = {"names": names, 
         "args_names": args_names, 
         "template_args_base": template_args_base,
         "template_statement_functorbase": template_statement_functorbase,
         "template_statement_functor": template_statement_functor,
         "template_statement_functor_void": template_statement_functor_void,
         "template_args_functor": template_args_functor,
         "template_args_functor_void": template_args_functor_void,
         "template_args_function": template_args_function,
         "template_args_function_void": template_args_function_void,
         "template_statement_function": template_statement_function,
         "template_statement_function_void": template_statement_function_void,
         "function_typename_statement": "typename",
         "postfix": str(num_args),
         "functorbase": FUNCTORBASE,
         "function": FUNCTION,
         "functor": FUNCTOR,
         "caller": CALLER,
         "memberfunction": MEMBERFUNCTION,
         "autoptrstart": AUTOPTR + "< " if USE_AUTOPTR else "",
         "autoptrend": " >" if USE_AUTOPTR else ""}

    if make_callers:
        caller_result = string.Template (caller_template).substitute (d)
    else:
        caller_result = ""

    if not USE_AUTOPTR:
      helper_template = helper_template_no_autoptr

    if make_helper:
        helper_result = string.Template (helper_template).substitute (d)
    else:
        helper_result = ""

    return s.substitute (d) + caller_result + helper_result

if len(sys.argv) < 2:
    print ("""
Make any amount of functor classes and helper functions, as well as caller classes.
Usage: %s <max. number of arguments>
""" % sys.argv[0])
    sys.exit ()

N = int (sys.argv[1])

s = """
//= Automatically created by mkfunctors.py.
//= Part of golib, (C) copyright 2009 by Christian Gosch
""" + docs + header

s += make_base (N)
# Make the bases 
for i in xrange (N):
    s += make_functors (i,N, base = True)
# Make voids:
for i in xrange (N):
    s += make_functors (i,N, base = False, make_helper = True, make_callers = True)

s += """
""" + footer
print s 
