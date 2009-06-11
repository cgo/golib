#!/usr/bin/python

import string, sys

"""
Helper program to create functor implementations up to a given max. number of arguments.
Versions for void and non-void are made, as well as matching helpers (goFunction() and goMemberFunction())
and goCaller classes.
Part of golib
(C) Copyright 2009 by Christian Gosch
"""

def make_base (max_args):
    args_string = "class Tret"
    for i in xrange (max_args):
        args_string += ", class Targ" + str(i)
    return """
template <""" + args_string + """>
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
"""

def make_functors (num_args, max_args, void_ret = False, base = False, make_callers = False, make_helper = False):

    template_base_string = """
${template_statement_functorbase}
class goFunctorBase${postfix} : public goFunctorBase <${template_args_base}> 
{
    public:
        goFunctorBase${postfix} ()
            : goFunctorBase <${template_args_base}> ()
        {
        }
        virtual ~goFunctorBase${postfix} ()
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
};
"""
    template_string = """
${template_statement_function}
class goFunction${postfix} : public goFunctorBase${postfix}<${template_args_function}>
{
    public:
        typedef Tret (*function_t)(${args_names});

    public:
        goFunction${postfix} (function_t function)
            : goFunctorBase${postfix}<${template_args_function}> (), myFunction (function)
        {
        }
        virtual ~goFunction${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (${args_names})
        {
            if (myFunction)
            {
                return (myFunction)(${names});
            }
        }
        virtual Tret operator () (${args_names}) const
        {
            if (myFunction)
            {
                return (myFunction)(${names});
            }
        }

    private:
        function_t myFunction;
};

${template_statement_function_void}
class goFunction${postfix}<${template_args_function_void}> : public goFunctorBase${postfix}<${template_args_function_void}>
{
    public:
        typedef void (*function_t)(${args_names});

    public:
        goFunction${postfix} (function_t function)
            : goFunctorBase${postfix}<${template_args_function_void}> (), myFunction (function)
        {
        }
        virtual ~goFunction${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (${args_names})
        {
            if (myFunction)
            {
                (myFunction)(${names});
            }
        }
        virtual void operator () (${args_names}) const
        {
            if (myFunction)
            {
                (myFunction)(${names});
            }
        }

    private:
        function_t myFunction;
};

${template_statement_functor}
class goFunctor${postfix} : public goFunctorBase${postfix}<${template_args_function}>
{
    public:
        typedef Tret (Tclass::*function_t)(${args_names});

    public:
        goFunctor${postfix} (Tclass* object, function_t function)
            : goFunctorBase${postfix}<${template_args_function}> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual Tret operator () (${args_names})
        {
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)(${names});
            }
        }
        virtual Tret operator () (${args_names}) const
        {
            if (myObject && myFunction)
            {
                return (myObject->*myFunction)(${names});
            }
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};

${template_statement_functor_void}
class goFunctor${postfix}<${template_args_functor_void}> : public goFunctorBase${postfix}<${template_args_function_void}>
{
    public:
        typedef void (Tclass::*function_t)(${args_names});

    public:
        goFunctor${postfix} (Tclass* object, function_t function)
            : goFunctorBase${postfix}<${template_args_function_void}> (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor${postfix} () {}

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual void operator () (${args_names})
        {
            if (myObject && myFunction)
            {
                (myObject->*myFunction)(${names});
            }
        }
        virtual void operator () (${args_names}) const
        {
            if (myObject && myFunction)
            {
                (myObject->*myFunction)(${names});
            }
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
 * @todo Fix the goList issue. golist.hpp must be included at the
 * end of one source file that uses this class.
 */
${template_statement_functorbase}
class goCaller${postfix}
{
    public:
        typedef goFunctorBase${postfix}<${template_args_function}> FunctorBase;
        typedef goList< goAutoPtr< FunctorBase > > FunctorList;

    public:
        goCaller${postfix} ()
            : fList () {}
        virtual ~goCaller${postfix} () {fList.erase ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr< FunctorBase > f)
        {
            this->fList.append (f);
        }

        void disconnect (goAutoPtr< FunctorBase > f)
        {
            typename FunctorList::Element* e = fList.find (f);
            if (e)
                fList.remove (e);
        }

        void clear ()
        {
            fList.clear ();
        }

        // virtual Tret operator () (${args_names})
        virtual void operator () (${args_names})
        {
            typename FunctorList::Element* el = this->fList.getFrontElement ();
            // Tret ret;
            while (el)
            {
                // ret = (*el->elem)(${names});
                (*el->elem)(${names});
                el = el->next;
            }
            // return ret;
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
 * @return goAutoPtr to a goFunctorBase${postfix} object.
 */
${template_statement_function}
goAutoPtr<goFunctorBase${postfix}<${template_args_function} > >
goFunction (${function_typename_statement} goFunction${postfix}<${template_args_function}>::function_t f)
{
    return goAutoPtr<goFunctorBase${postfix}<${template_args_function}> > (new goFunction${postfix}<${template_args_function}> (f));
}

/** 
 * @brief Create a member function functor object, encapsulated
 * in a goAutoPtr for automatic deletion.
 * 
 * @param c  Pointer to the object.
 * @param f  Function pointer to a member function.
 * 
 * @return goAutoPtr to a goFunctorBase${postfix} object.
 */
${template_statement_functor}
// template <class Tclass, class Tret, class Targ1>
goAutoPtr<goFunctorBase${postfix}<${template_args_function} > >
goMemberFunction (Tclass* c, typename goFunctor${postfix}<${template_args_functor}>::function_t f)
{
    return goAutoPtr<goFunctorBase${postfix}<${template_args_function}> > (static_cast<goFunctorBase${postfix}<${template_args_function}>*> (new goFunctor${postfix}<${template_args_functor}> (c, f)));
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
         "postfix": str(num_args)}

    if make_callers:
        caller_result = string.Template (caller_template).substitute (d)
    else:
        caller_result = ""

    if make_helper:
        helper_result = string.Template (helper_template).substitute (d)
    else:
        helper_result = ""

    return s.substitute (d) + caller_result + helper_result

if len(argv) < 2:
    print ("""
Make any amount of functor classes and helper functions, as well as goCaller classes.
Usage: %s <max. number of arguments>" % argv[0])
""")
    sys.exit ()

N = int (argv[1])

s = make_base (N)
# Make the bases 
for i in xrange (N):
    s += make_functors (i,N, base = True)
# Make voids:
for i in xrange (N):
    s += make_functors (i,N, base = False, make_helper = True, make_callers = True)
print s 
