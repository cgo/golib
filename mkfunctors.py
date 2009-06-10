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
"""

def make_functors (num_args, max_args, void_ret = False, base = False):

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
    else:
        template_statement_functor = "template <class Tret, class Tclass"
    if (void_ret and num_args <= 0):
        template_statement_function = ""
        template_suffix = "<void>"
    else:
        template_statement_function = "template <"
        template_suffix = "<"
        if (void_ret):
            template_suffix += "void"
        else:
            template_suffix += "Tret"
        if num_args > 0:
            template_suffix += ", "
        if (not void_ret):
            template_statement_function += "class Tret"
            if num_args > 0:
                template_statement_function += ", "
        if num_args > 0:
            template_statement_function += "class Targ0"
            template_statement_functor += ", class Targ0"
            template_suffix += "Targ0"
            for i in xrange (1, num_args):
                template_statement_function += ", class Targ" + str(i)
                template_statement_functor += ", class Targ" + str(i)
                template_suffix += ", Targ" + str(i)
        template_statement_function += ">"
        template_suffix += ">"
    template_statement_functor += ">"    

    if (base):
        s = string.Template (template_base_string)
    else:
        s = string.Template (template_string)

    d = {"names": names, 
         "args_names": args_names, 
         "template_args_base": template_args_base,
         "template_suffix": template_suffix,
         "template_statement_functorbase": template_statement_functorbase,
         "template_statement_functor": template_statement_functor,
         "template_statement_function": template_statement_function,
         "ret": "Tret",
         "return_statement": "return",
         "postfix": str(num_args),
         "functionpostfix": "",
         "functorpostfix" : ""}
    if (void_ret):
        d["ret"] = "void"
        d["return_statement"] = ""
        d["functionpostfix"] = "Void"
        d["functorpostfix"] = "Void"
    return s.substitute (d)

s = make_base (10)
# Make the bases (void_ret does not matter):
for i in xrange (3):
    s += make_functors (i,10,void_ret = True, base = True)
# Make voids:
for i in xrange (3):
    s += make_functors (i,10,void_ret = True, base = False)
# Make non-voids:
for i in xrange (3):
    s += make_functors (i,10,void_ret = False, base = False)
print s 
