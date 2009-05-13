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

#include <vector>

namespace goMath
{
/** \addtogroup mathopt
 * @{
 */

    
     /** 
     * @brief Backtracking line search.
     * 
     * Searches for a sufficient decrease of f(x) in direction -dx, by repeating
     * \f[ t = \beta \, t \f]
     * until
     * \f[ f(x - t \, \Delta x) > f(x) - \alpha \, t \, \nabla f(x)^\top \Delta x \f]
     * with
     * \f[ \alpha \in (0, 0.5), \, \beta \in (0,1) \, . \f]
     *
     * @par References
     * Boyd, S. & Vandenberghe, L.: 
     * Convex Optimization. Cambridge University Press, 2004
     *
     * @note \f$x - t \, \Delta x\f$ must be in the domain of \c f,
     * so in some cases it may be necessary to choose \c t accordingly
     * when calling this function.
     *
     * @param callable_ Some callable object, i.e. allowing 
     *  <code>vector_type::value_type callable_::operator() (const vector_type&)</code>
     * @param vector_type A vector type, e.g. goMath::Vector<>
     *
     * @param f Function to evaluate
     * @param f_x Value at point \c x
     * @param nabla_f_x Value of the gradient of \c f at \c x
     * @param x Point at which to evaluate
     * @param dx Direction in which to evaluate
     * @param t Starting parameter \c t, defaults to 1
     * @param alpha Parameter \c alpha, defaults to 0.2
     * @param beta Parameter \c beta, defaults to 0.8
     * 
     * @return The value of \c t so that f(x - t*dt) is sufficiently decreasing.
     */
    template <class callable_, class vector_type>
    typename vector_type::value_type backtrackingLineSearch (callable_& f,
                                  typename vector_type::value_type f_x,
                                  const vector_type& nabla_f_x,
                                  const vector_type& x, 
                                  const vector_type& dx, 
                                  typename vector_type::value_type t = 1, 
                                  typename vector_type::value_type alpha = 0.2, 
                                  typename vector_type::value_type beta = 0.8)
    {
        typename vector_type::value_type Nfx_times_dx = nabla_f_x * dx;

        typename vector_type::value_type fx2 = f (x - dx * t);

        while ( t > 1e-6 && 
                fx2 > f_x - Nfx_times_dx * alpha * t )
        {
            t = beta * t;
            fx2 = f (x - dx * t);
        } 

        if (fx2 <= f_x - Nfx_times_dx * alpha * t)
            return t;
        else
            return typename vector_type::value_type (0.0);
    }

    template <class callable_, class vector_type>
    class LineSearch
    {
        public:
            LineSearch (typename vector_type::value_type t = 1, typename vector_type::value_type alpha = 0.2, typename vector_type::value_type beta = 0.5)
                : myT (t), myAlpha (alpha), myBeta (beta)
            { 
            }

            virtual ~LineSearch ()
            {
            }

            typename vector_type::value_type operator() (callable_& f,
                                                typename vector_type::value_type f_x,
                                                const vector_type& nabla_f_x,
                                                const vector_type& x, 
                                                const vector_type& dx)
            {
                return backtrackingLineSearch<callable_, vector_type> (f, f_x, nabla_f_x, x, dx, myT, myAlpha, myBeta);
            }

        protected:
            typename vector_type::value_type myT;
            typename vector_type::value_type myAlpha;
            typename vector_type::value_type myBeta;
    };

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

                virtual value_type operator () (const vector_type& x) = 0; 

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
                    virtual typename parent::value_type operator () (const vector_type& x) 
                    {
                        return (*myF) (x);
                    }

                private:
                    goAutoPtr<goFunctorBase1 <typename parent::value_type, const vector_type&> > myF;
            };


        template <class matrix_type, class vector_type>
            class OptProblem
            {
                public:
                    typedef typename matrix_type::value_type value_type;
                    typedef OptFunction <matrix_type, vector_type> function_type;
                    
                public:
                    OptProblem (goAutoPtr<function_type> f)
                        : myF (f),
                          myIneqCon (),
                          myEqCon_A (0),
                          myEqCon_b (0),
                          myNonNegativity (false)
                    {
                    }

                    virtual ~OptProblem ()
                    {
                    }

                    void setNonNegativity (bool val = true)
                    {
                        myNonNegativity = val;
                    }

                    bool nonNegativity () const
                    {
                        return myNonNegativity;
                    }

                    void addIneqCon (goAutoPtr<function_type> f)
                    {
                        myIneqCon.push_back (f);
                    }

                    void setEqCon (goAutoPtr<matrix_type> A, goAutoPtr<vector_type> b)
                    {
                        myEqCon_A = A;
                        myEqCon_b = b;
                    }

                    goAutoPtr<function_type> f ()
                    {
                        return myF;
                    }

                    goAutoPtr<function_type> ineq (goSize_t i)
                    {
                        return myIneqCon[i];
                    }

                    goSize_t ineqCount () const
                    {
                        return myIneqCon.size ();
                    }

                    goAutoPtr<matrix_type> eqA ()
                    {
                        return myEqCon_A;
                    }

                    goAutoPtr<vector_type> eqB ()
                    {
                        return myEqCon_b;
                    }

                private:
                    goAutoPtr<function_type>           myF;
                    std::vector<goAutoPtr<function_type> >  myIneqCon;
                    goAutoPtr<matrix_type>  myEqCon_A;
                    goAutoPtr<vector_type>  myEqCon_b;

                    bool myNonNegativity; // If true, the solution x must be non-negative.
            };
/** @} */
};

#endif
