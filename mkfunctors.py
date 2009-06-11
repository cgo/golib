import string

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
class goFunction${postfix}${functionpostfix} : public goFunctorBase${postfix}${template_suffix}
{
    public:
        typedef ${ret} (*function_t)(${args_names});

    public:
        goFunction${postfix}${functionpostfix} (function_t function)
            : goFunctorBase${postfix}${template_suffix} (), myFunction (function)
        {
        }
        virtual ~goFunction${postfix}${functionpostfix} () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual ${ret} operator () (${args_names})
        {
            if (myFunction)
            {
                ${return_statement} (myFunction)(${names});
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        }
        virtual ${ret} operator () (${args_names}) const
        {
            if (myFunction)
            {
                ${return_statement} (myFunction)(${names});
            }
        }

    private:
        function_t myFunction;
};

${template_statement_function_void}
class goFunction${postfix} : public goFunctorBase${postfix}${template_suffix_void}
{
    public:
        typedef void (*function_t)(${args_names});

    public:
        goFunction${postfix} (function_t function)
            : goFunctorBase${postfix}${template_suffix_void} (), myFunction (function)
        {
        }
        virtual ~goFunction${postfix} () {};

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
class goFunctor${postfix}${functorpostfix} : public goFunctorBase${postfix}${template_suffix}
{
    public:
        typedef ${ret} (Tclass::*function_t)(${args_names});

    public:
        goFunctor${postfix}${functorpostfix} (Tclass* object, function_t function)
            : goFunctorBase${postfix}${template_suffix} (), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor${postfix}${functorpostfix} () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual ${ret} operator () (${args_names})
        {
            if (myObject && myFunction)
            {
                ${return_statement} (myObject->*myFunction)(${names});
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        }
        virtual ${ret} operator () (${args_names}) const
        {
            if (myObject && myFunction)
            {
                ${return_statement} (myObject->*myFunction)(${names});
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
        typedef goList< goAutoPtr<goFunctorBase${postfix}${template_suffix} > > FunctorList;

    public:
        goCaller${postfix} ()
            : fList () {}
        virtual ~goCaller${postfix} () {fList.erase ();}

        //= Take very much care here that
        //= 1. This class gets notified if functors are destroyed.
        //= 2. Everything is thread safe.
        void connect (goAutoPtr<goFunctorBase${postfix}${template_suffix} > f)
        {
            this->fList.append (f);
        }

        void disconnect (goAutoPtr<goFunctorBase${postfix}${template_suffix} > f)
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
goAutoPtr<goFunctorBase${postfix}${template_suffix} >
goFunction (${function_typename_statement} goFunction${postfix}${functionpostfix}${template_suffix_function}::function_t f)
{
    return goAutoPtr<goFunctorBase${postfix}${template_suffix} > (new goFunction${postfix}${functionpostfix}${template_suffix_function} (f));
     // (static_cast<goFunctorBase${postfix}${template_suffix}*> (new goFunction${postfix}${template_suffix_function} (f)));
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
goAutoPtr<goFunctorBase${postfix}${template_suffix} >
goMemberFunction (Tclass* c, typename goFunctor${postfix}${functorpostfix}<${template_args_functor}>::function_t f)
{
    return goAutoPtr<goFunctorBase${postfix}${template_suffix} > (static_cast<goFunctorBase${postfix}${template_suffix}*> (new goFunctor${postfix}${functorpostfix}<${template_args_functor}> (c, f)));
}
"""

    args_string = ""
    args_names_string = ""
    names_string = ""
#    if ret == "void":
#        void_ret = True
#    else:
#        void_ret = False

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
    
    if (void_ret):
        template_statement_functor = "template <class Tclass"
        template_args_functor = "Tclass"
    else:
        template_statement_functor = "template <class Tret, class Tclass"
        template_args_functor = "Tret, Tclass"
    if (void_ret and num_args <= 0):
        template_statement_function = ""
        template_suffix_function = ""
        template_suffix = "<void>"
    else:
        template_statement_function = "template <"
        template_suffix = "<"
        template_suffix_function = "<"
        if (void_ret):
            template_suffix += "void"
        else:
            template_suffix += "Tret"
        if num_args > 0:
            template_suffix += ", "
        if (not void_ret):
            template_statement_function += "class Tret"
            template_suffix_function += "Tret"
            if num_args > 0:
                template_statement_function += ", "
                template_suffix_function += ", "
        if num_args > 0:
            template_statement_function += "class Targ0"
            template_statement_functor += ", class Targ0"
            template_args_functor += ", Targ0"
            template_suffix += "Targ0"
            template_suffix_function += "Targ0"
            for i in xrange (1, num_args):
                template_statement_function += ", class Targ" + str(i)
                template_statement_functor += ", class Targ" + str(i)
                template_args_functor += ", Targ" + str(i)
                template_suffix += ", Targ" + str(i)
                template_suffix_function += ", Targ" + str(i)
        template_statement_function += ">"
        template_suffix += ">"
        template_suffix_function += ">"
    template_statement_functor += ">"    

    if (base):
        s = string.Template (template_base_string)
    else:
        s = string.Template (template_string)

    d = {"names": names, 
         "args_names": args_names, 
         "template_args_base": template_args_base,
         "template_suffix": template_suffix,
         "template_suffix_function": template_suffix_function,
         "template_statement_functorbase": template_statement_functorbase,
         "template_statement_functor": template_statement_functor,
         "template_args_functor": template_args_functor,
         "template_statement_function": template_statement_function,
         "function_typename_statement": "typename",
         "ret": "Tret",
         "return_statement": "return",
         "postfix": str(num_args),
         "functionpostfix": "",
         "functorpostfix" : ""}

    if template_suffix_function == "":
        d["function_typename_statement"] = ""

    if make_callers:
        caller_result = string.Template (caller_template).substitute (d)
    else:
        caller_result = ""

    if (void_ret):
        d["ret"] = "void"
        d["return_statement"] = ""
        d["functionpostfix"] = "Void"
        d["functorpostfix"] = "Void"

    if make_helper:
        helper_result = string.Template (helper_template).substitute (d)
    else:
        helper_result = ""

    return s.substitute (d) + caller_result + helper_result

s = make_base (10)
# Make the bases (void_ret does not matter):
for i in xrange (3):
    s += make_functors (i,10,void_ret = True, base = True)
# Make voids:
for i in xrange (3):
    s += make_functors (i,10,void_ret = True, base = False, make_helper = True)
# Make non-voids:
for i in xrange (3):
    s += make_functors (i,10,void_ret = False, base = False, make_callers = True, make_helper = True)
print s 
