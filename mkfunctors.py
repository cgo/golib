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

def make_functors (num_args, max_args):

    def make_template (ret):
        args_string = ""
        args_names_string = ""
        names_string = ""
        if ret == "void":
            void_ret = True
        else:
            void_ret = False

        args_string2 = ""

        for i in xrange (num_args):
            if i == 0:
                args_string += "class Targ" + str(i)
                args_string2 += "Targ" + str(i)
            else:
                args_string += ", class Targ" + str(i)
                args_string2 += ", Targ" + str(i)
            if i > 0:
                args_names_string += ", Targ" + str(i) + " arg" + str(i)
                names_string += ", arg" + str(i)
            else:
                args_names_string += "Targ" + str(i) + " arg" + str(i)
                names_string += "arg" + str(i)
        
        if void_ret:
            template_args = args_string
            template_args_functor = """class Tclass""" + args_string

            if num_args > 0:
                template_args_base = args_string2
            else:
                template_args_base = 
        else:
            template_args = ret + ", " + args_string
            template_args_functor = ret + """, class Tclass, """ + args_string

            if num_args > 0:
                template_args_base = ret + ", " + args_string2
            else:
                template_args_base = ret

        rest_string = ""
        for i in xrange (max_args - num_args):
            rest_string += ",void"

        template_string = """
template <""" + template_args + """>
class goFunctorBase${postfix} : public goFunctorBase <""" + template_args_base + rest_string + ">" + """ 
{
    public:
        goFunctorBase${postfix} ()
            : goFunctorBase <""" + template_args_base + rest_string + """> ()
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
        virtual """ + ret + """ operator () () = 0;
        virtual """ + ret + """ operator () () const = 0;
};


template <""" + template_args + """>
class goFunction${postfix} : public goFunctorBase${postfix}<""" + template_args_base + """>
{
    public:
        typedef """ + ret + """ (*function_t)(""" + args_names_string + """);

    public:
        goFunction${postfix} (function_t function)
            : goFunctorBase${postfix}<""" + template_args_base + """>(), myFunction (function)
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
        virtual """ + ret + """ operator () (""" + args_names_string + """)
        {
            if (myFunction)
            {
                ${return_statement} (myFunction)(""" + names_string + """);
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        }
        virtual """ + ret + """ operator () (""" + args_names_string + """) const
        {
            if (myFunction)
            {
                ${return_statement} (myFunction)(""" + names_string + """);
            }
        }

    private:
        function_t myFunction;
};

template <""" + template_args_functor + """>
class goFunctor${postfix} : public goFunctorBase${postfix}<""" + template_args_base + """>
{
    public:
        typedef """ + ret + """(Tclass::*function_t)(""" + args_names_string + """);

    public:
        goFunctor${postfix} (Tclass* object, function_t function)
            : goFunctorBase${postfix}<""" + template_args_base + """>(), myObject (object), myFunction (function)
        {
        }
        virtual ~goFunctor${postfix} () {};

        /** 
         * @brief Calls the function set to this functor.
         *
         * @param p Whatever parameter the represented function takes.
         *
         * @return Whatever the set function returns.
         */
        virtual ${ret} operator () (""" + args_names_string + """)
        {
            if (myObject && myFunction)
            {
                ${return_statement} (myObject->*myFunction)(""" + names_string + """);
            }
            //else
            //{
            //    Tret dummy;
            //    return dummy;
           // }
        }
        virtual ${ret} operator () (""" + args_names_string + """) const
        {
            if (myObject && myFunction)
            {
                ${return_statement} (myObject->*myFunction)(""" + names_string + """);
            }
        }

    private:
        Tclass* myObject;
        function_t myFunction;
};
"""
        return template_string
    
    s = string.Template (make_template ("Tret"))
    svoid = string.Template (make_template ("void"))
    d = {"class_ret": "class Tret", "ret": "Tret", "return_statement": "return", "postfix": str(num_args)}
    dvoid = {"class_ret": "", "ret": "void", "return_statement": "", "postfix": str(num_args)}
    return svoid.substitute (dvoid) + s.substitute (d)

print make_base (10) + make_functors (1,10) + make_functors (2,10)
