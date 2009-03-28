#ifndef GOOPT_H
#define GOOPT_H

#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOLAPACK_H
# include <golapack.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

namespace goMath
{
/** \addtogroup mathopt
 * @{
 */
     /** 
     * @brief Function interface for Newton type optimisation.
     *
     * @see NewtonOpt, NewtonOptEq
     *
     * Provides function evaluation, gradient, and Hessian computation.
     */
    template <class matrix_type, class vector_type>
        class OptFunction
        {
            public:
                /** 
                * @brief Value of the matrix and vector types matrix_type and vector_type.
                */
                typedef typename vector_type::value_type value_type;

                virtual ~OptFunction () 
                { }

                virtual value_type operator () (const vector_type& x) const = 0; 

                /** 
                 * @brief Calculate the gradient of f at x.
                 * 
                 * Does a numerical approximation with forward differences.
                 * Re-implement for specialisations.
                 *
                 * @note x is modified during the call, but restored when the call returns.
                 *
                 * @param x Point at which to calculate the gradient
                 * @param ret Contains \f$\nabla f(x)\f$ on return
                 */
                virtual void grad (vector_type& x, vector_type& ret)
                {
                    const goSize_t sz = x.getSize ();
                    if (ret.getSize () != sz)
                    {
                        ret.setSize (sz);  //= setSize is faster than resize.
                    }

                    double factor = 1.0 / myEpsilon;
                    value_type temp = value_type (0);
                    value_type temp2 = value_type (0);
                    value_type fx = this->operator()(x);

                    for (goSize_t i = 0; i < sz; ++i)
                    {
                        temp = x[i];
                        x[i] += myEpsilon;
                        temp2 = this->operator() (x);
                        x[i] = temp;       // Restore x[i]
                        ret[i] = (temp2 - fx) * factor;
                    }
                }

                /** 
                 * @brief Calculate the Hessian of f at x.
                 * 
                 * Does a numerical approximation with forward differences.
                 * Re-implement for specialisations.
                 *
                 * @note x is modified during the call, but restored when the call returns.
                 *
                 * @param x Point at which to calculate the Hessian
                 * @param ret Contains \f$H(f(x))\f$ on return
                 */
                virtual void hessian (vector_type& x, matrix_type& ret)
                {
                    // H_{i,j} = d^2f / (dx_i dx_j)
                    const goSize_t sz = x.getSize ();
                    if (ret.getColumns () != sz || ret.getRows () != sz)
                    {
                        ret.resize (sz, sz);  //= setSize is faster than resize.
                    }

                    //= precompute f(x_1,...,x_i + epsilon,...,x_n)
                    vector_type f_xi (sz);
                    value_type temp = value_type (0);

                    for (goSize_t i = 0; i < sz; ++i)
                    {
                        temp = x[i];
                        x[i] += myEpsilon;
                        f_xi[i] = this->operator() (x);
                        x[i] = temp;
                    }

                    value_type fx = this->operator() (x);

                    double factor = 1.0 / (myEpsilon * myEpsilon);

                    for (goSize_t i = 0; i < sz; ++i)
                    {
                        temp = x[i];
                        x[i] += myEpsilon;
                        value_type temp2 = value_type (0);

                        for (goSize_t j = i; j < sz; ++j)
                        {
                            temp2 = x[j];
                            x[j] += myEpsilon;
                            ret (i,j) = ((this->operator() (x) - f_xi[j]) - (f_xi[i] - fx)) * factor;
                            x[j] = temp2; // Restore x[j]
                            ret (j,i) = ret (i,j);
                        }
                        x[i] = temp;  // Restore x[i]
                    }
                }
            
            protected:
                /** 
                * @brief Constructor.
                * 
                * @param eps Epsilon, used in numerically calculating the gradient and Hessian.
                */
                explicit OptFunction (double eps = 0.01)
                    : myEpsilon (eps)
                { }

            private:
                double myEpsilon;
        };


        /** 
        * @brief Convenience class taking a functor and providing the OptFunction interface.
        *
        * @see NewtonOpt, NewtonOptEq, OptFunction
        *
        * If you do not want to derive a new class for each function you want to minimise,
        * use this in conjunction with the goFunctorBase and goFunctor related classes
        * to provide an interface compatible with the NewtonOpt family of classes.
        *
        * @param matrix_type Matrix type, typically goMath::Matrix
        * @param vector_type Vector type, typically goMath::Vector
        */
        template <class matrix_type, class vector_type>
            class OptFunctor : public OptFunction <matrix_type, vector_type>
            {
                public:
                    typedef OptFunction<matrix_type, vector_type> parent;

                public:
                /** 
                * @brief Constructor.
                * 
                * @param parent::value_type The return type of the method or function, i.e. the type of the
                * given matrix_type and vector_type types.
                * @param f The functor to minimise
                * @param eps Epsilon, see OptFunction::OptFunction()
                */
                    OptFunctor (goAutoPtr<goFunctorBase1 <typename parent::value_type, const vector_type&> > f, 
                        double eps = 0.01)
                        : OptFunction <matrix_type, vector_type> (eps),
                          myF (f)
                    {
                    }

                    virtual ~OptFunctor () { }

                    /** 
                    * @brief Evaluates the given functor at x.
                    * 
                    * @return f(x), the value of the functor at x
                    */
                    virtual typename parent::value_type operator () (const vector_type& x) const
                    {
                        return (*myF) (x);
                    }

                private:
                    goAutoPtr<goFunctorBase1 <typename parent::value_type, const vector_type&> > myF;
            };


        template <class callable_, class matrix_type, class vector_type>
            class OptProblem
            {
                public:
                    typedef callable_::value_type value_type;
                    
                public:
                    OptProblem (goAutoPtr<callable_> f)
                        : myF (f),
                          myIneqCon (),
                          myEqCon_A (0),
                          myEqCon_b (0)
                    {
                    }

                    virtual ~OptProblem ()
                    {
                    }

                    void addIneqCon (goAutoPtr<callable_> f)
                    {
                        myIneqCon.push_back (f);
                    }

                    void setEqCon (goAutoPtr<matrix_type> A, goAutoPtr<vector_type> b)
                    {
                        myEqCon_A = A;
                        myEqCon_b = b;
                    }

                    goAutoPtr<callable_>& f ()
                    {
                        return myF;
                    }

                    goAutoPtr<callable_>& ineq (goSize_t i)
                    {
                        return myIneqCon[i];
                    }

                    goSize_t ineqCount () const
                    {
                        return myIneqCon.size ();
                    }

                    goAutoPtr<matrix_type>& eqA ()
                    {
                        return myEqCon_A;
                    }

                    goAutoPtr<matrix_type>& eqB ()
                    {
                        return myEqCon_b;
                    }

                private:
                    goAutoPtr<callable_>                myF;
                    std::vector<goAutoPtr<callable_> >  myIneqCon;
                    goAutoPtr<matrix_type>  myEqCon_A;
                    goAutoPtr<vector_type>  myEqCon_b;
            };
/** @} */
};

#endif
