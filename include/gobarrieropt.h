/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOBARRIEROPT_H
#define GOBARRIEROPT_H

#ifndef GOOPT_H
# include <goopt.h>
#endif
#ifndef GONEWTON_H
# include <gonewton.h>
#endif
#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOLAPACK_H
# include <golapack.h>
#endif

#include <limits>

#include <goconfig.h>

//= For FE tra (debugging), some functions not available in OSX.
extern "C" {
 #include <fenv.h>
 #include <signal.h>
}

namespace goMath
{
   //= Idee: BarrierOpt -> ruft NewtonOptEq mit BarrierOptFunction als 
   //= OptFunction (callable_) auf.
   //= Die Auessere Schleife laeuft in BarrierOpt.    

    //= FIXME: NonNegativity flag in OptProblem benutzen, um log barrier
    //=        so zu erweitern, dass x_i >= 0 erzwungen wird.

    /** 
     * @brief An OptFunction that is used in BarrierOpt.
     *
     * This class provides gradient and Hessian methods for calculating
     * the gradient and Hessian of an optimisation problem
     * set with \c setProblem() by augmenting the gradient and Hessian
     * of the original objective function by the ones induced by the log barrier
     * function.
     *
     * The gradient is calculated as ... FIXME
     * 
     */
   template <class matrix_type, class vector_type>
        class BarrierOptFunction : public OptFunction <matrix_type, vector_type>
   {
       public:
           typedef typename OptFunction<matrix_type,vector_type>::value_type value_type;
           typedef OptFunction<matrix_type,vector_type> function_type;

       public:
           BarrierOptFunction (double eps = 0.01)
               : OptFunction <matrix_type,vector_type> (eps),
               myProblem (0),
               myBufferGrad (0),
               myBufferHess (0, 0),
               my_t (value_type (1))
           {
           }

           virtual ~BarrierOptFunction ()
           {
           }

           void setT (value_type t)
           {
               my_t = t;
           }

           value_type t () const
           {
               return my_t;
           }


           /** 
           * @brief Barrier function \f$ \Phi \f$
           * 
           * Calculates the barrier function
           * \f[ \Phi(x) = -\sum_{i=1}^{m} \log (-f_i(x)) \f]
           * for all inequality constraints \f$ f_i(x) \leq 0 \f$ which obviously must be met
           * \b strictly.
           *
           * @param x Point at which to calculate \f$ \Phi(x) \f$.
           */
           virtual value_type barrier (const vector_type& x)
           {
               value_type sumLog = value_type (0);
               goSize_t M = myProblem->ineqCount ();

               value_type op = value_type (0); 

               for (goSize_t i = 0; i < M; ++i)
               {
                   op = -myProblem->ineq(i)->operator() (x);
                   printf ("BarrierOptFunction::barrier(): -f_%ld(x) = %f\n", i, op);
                   if (op > value_type (0))
                   {
                       sumLog -= ::log (op);
                   }
                   else
                   {
                       //= Return "something large"
                       return std::numeric_limits<value_type>::max () * value_type (0.5);
                   }
               }

               if (myProblem->nonNegativity ())
               {
                   const goSize_t N = x.getSize ();
                   for (goSize_t i = 0; i < N; ++i)
                   {
                       if (x[i] > value_type(0))
                       {
                           sumLog -= ::log (x[i]);
                       }
                       else
                       {
                           //= Return "something large"
                           return std::numeric_limits<value_type>::max () * value_type (0.5);
                       }
                   }
               }

               return sumLog;
           }

           void setProblem (goAutoPtr<OptProblem <matrix_type, vector_type> > p)
           {
               myProblem = p;
           }

           /** 
           * @brief Calculate \f$ t \cdot f(x) + \Phi(x) \f$
           * 
           * @see barrier()
           *
           * \f$ f(x) \f$ is the original function to be minimised, \f$ \Phi(x) \f$ is the
           * log barrier.
           *
           * @return \f$ t \cdot f(x) + \Phi(x) \f$
           */
           virtual value_type operator () (const vector_type& x) 
           {
               return this->my_t * (*(myProblem->f()))(x) + this->barrier (x);
           }

           virtual void grad (vector_type& x, vector_type& ret)
           {
               const goSize_t M = myProblem->ineqCount ();
               const goSize_t N = x.getSize ();

               if (ret.getSize() != N)
               {
                   ret.setSize (N);
               }

               ret.fill (value_type (0));


               value_type fi_x = value_type (0);
               for (goSize_t i = 0; i < M; ++i)
               {
                   //== nabla phi(x) = sum_{i=1}^{M} 1/-f_i(x) \, nabla f_i(x)
                   //= 1 / f_i(x)
                   fi_x = value_type(1) / (*myProblem->ineq(i))(x);
                   myProblem->ineq(i)->grad (x, myBufferGrad);
                   goMath::vectorAdd (-fi_x, myBufferGrad, ret);
               }

               if (myProblem->nonNegativity ())
               {
                   for (goSize_t i = 0; i < N; ++i)
                   {
                       ret[i] -= (x[i] != 0.0) ? (1.0 / x[i]) : 0.0;
                   }
               }

               myProblem->f()->grad (x, myBufferGrad);
               goMath::vectorAdd (my_t, myBufferGrad, ret);
           }

           virtual void hessian (vector_type& x, matrix_type& ret)
           {
               const goSize_t M = myProblem->ineqCount ();
               const goSize_t N = x.getSize ();

               if (myBufferGrad.getSize() != N)
               {
                   myBufferGrad.setSize (N);
               }

               if (ret.getColumns() != N || ret.getRows() != N)
               {
                   ret.resize (N, N);
               }

               myBufferGrad.fill (value_type (0));
               ret.fill (value_type (0));
               // myBufferGrad.fill (value_type(0));

               //= Boyd eq. (11.14)

               value_type fi_x = value_type (0);
               for (goSize_t i = 0; i < M; ++i)
               {
                   //== nabla phi(x) = sum_{i=1}^{M} 1/-f_i(x) \, nabla f_i(x)
                   //= 1 / f_i(x)
                   fi_x = value_type(1) / (*myProblem->ineq(i))(x);
                   myProblem->ineq(i)->grad (x, myBufferGrad);

                   //= sum 1/f_i(x)^2 nabla f_i(x) (nabla f_i(x))^\top
                   goMath::vectorOuter<value_type> (fi_x * fi_x, myBufferGrad, myBufferGrad, ret);

                   myProblem->ineq(i)->hessian (x, myBufferHess);
                   myBufferHess *= -fi_x;
                   ret += myBufferHess;
               }

               //= FIXME: This can be done more efficiently.
               if (myProblem->nonNegativity ())
               {
                   for (goSize_t i = 0; i < N; ++i)
                   {
                       value_type f = (x[i] != 0.0) ? (1.0 / (x[i] * x[i])) : 0.0;
                       ret (i,i) += f;
                   }
               }

               myProblem->f()->hessian (x, myBufferHess);
               myBufferHess *= my_t;
               ret += myBufferHess;
           }

           //= Calculate gradient and Hessian of the logarithmic barrier function
           void barrierGradHessian (const vector_type& x, vector_type& grad, matrix_type& hess)
           {
               const goSize_t M = myProblem->ineqCount ();
               const goSize_t N = x.getSize ();

               if (grad.getSize() != N)
               {
                   grad.setSize (N);
               }

               if (hess.getColumns() != N || hess.getRows() != N)
               {
                   hess.resize (N, N);
               }

               grad.fill (value_type (0));
               hess.fill (value_type (0));
               // myBufferGrad.fill (value_type(0));

               value_type fi_x = value_type (0);
               for (goSize_t i = 0; i < M; ++i)
               {
                   //== nabla phi(x) = sum_{i=1}^{M} 1/-f_i(x) \, nabla f_i(x)
                   //= 1 / f_i(x)
                   fi_x = value_type(1) / (*myProblem->ineq(i))(x);
                   myProblem->ineq(i)->grad (x, myBufferGrad);
                   goMath::vectorAdd (-fi_x, myBufferGrad, grad);

                   //= sum 1/f_i(x)^2 nabla f_i(x) (nabla f_i(x))^\top
                   goMath::vectorOuter<value_type> (fi_x * fi_x, myBufferGrad, myBufferGrad, hess);

                   myProblem->ineq(i)->hessian (x, myBufferHess);
                   myBufferHess *= -fi_x;
                   hess += myBufferHess;
               }
               //= FIXME if this gets used, add hessian and grad of myProblem->f().
           }

           goAutoPtr<OptProblem<matrix_type, vector_type> >  problem ()
           {
               return myProblem;
           }

       protected:
           goAutoPtr<OptProblem<matrix_type, vector_type> > myProblem;
           vector_type myBufferGrad;
           matrix_type myBufferHess;

           value_type my_t;
   };

   //= FIXME: grad und hessian implementieren -- mit Hilfe von BarrierOptFunction::grad|hessian plus Faktor s.
   #if 1
   template <class matrix_type, class vector_type>
       class BarrierOptFunctionPhase1 : public BarrierOptFunction<matrix_type, vector_type>
   {
       public:
           typedef BarrierOptFunction<matrix_type, vector_type> parent;
           typedef typename matrix_type::value_type value_type;

           BarrierOptFunctionPhase1 (double eps = 0.01)
               : BarrierOptFunction <matrix_type, vector_type> (eps),
                 myVector (0)
           {
           }

           virtual ~BarrierOptFunctionPhase1 ()
           {
           }

           /** 
           * @brief Barrier function \f$ \Phi \f$
           * 
           * Calculates the barrier function
           * \f[ \Phi(x,s) = -\sum_{i=1}^{m} \log (-f_i(x) + s) \f]
           * for all inequality constraints \f$ f_i(x) \leq 0 \f$ which obviously must be met
           * \b strictly.
           *
           * @param x_s Point at which to calculate \f$ \Phi(x,s) \f$.
           * The last entry of vector \c x_s is \c s.
           */
           virtual value_type barrier (const vector_type& x_s)
           {
               value_type sumLog = value_type (0);
               goSize_t M = this->myProblem->ineqCount ();

               goSize_t N = x_s.getSize ();

               value_type s = x_s [N - 1];
               const vector_type x (0);
               //= Rather ugly but we need this to let x_s be const (as it should be)
               (const_cast<vector_type*> (&x))->setData (const_cast<value_type*> (x_s.getPtr()), N - 1, x_s.getStride());

               value_type op = value_type (0);

               for (goSize_t i = 0; i < M; ++i)
               {
                   op = -(this->myProblem->ineq(i)->operator() (x) - s);
                   // printf ("BarrierOptFunctionPhase1::barrier(): -f_%d(x) + s == %f\n", i, op);
                   if (op > value_type (0))
                   {
                       sumLog -= ::log (op);
                   }
                   else
                   {
                       //= "Something large"
                       // sumLog += std::numeric_limits<value_type>::max () / float (M + 1);
                       return std::numeric_limits<value_type>::max () * value_type (0.5);
                   }
               }

               return sumLog;
           }


           /** 
           * @brief Calculate \f$ t \cdot s + \Phi(x) \f$
           * 
           * @see barrier()
           *
           * \f$ f(x) \f$ is the original function to be minimised, \f$ \Phi(x) \f$ is the
           * log barrier.
           *
           * @return \f$ t \cdot f(x) + \Phi(x) \f$
           */
           virtual value_type operator () (const vector_type& x_s)
           {
               // const_cast<vector_type*> (&myVector) -> setData (const_cast<value_type*> (x.getPtr()), x.getSize() - 1, x.getStride ());
               //= s + log barrier
               return this->t() * x_s[x_s.getSize() - 1] + this->barrier (x_s);
           }

           //= Last entry of x_s is s
           virtual void grad (vector_type& x_s, vector_type& ret)
           {
               const goSize_t M = this->myProblem->ineqCount ();

               goSize_t x_s_sz = x_s.getSize ();

               if (ret.getSize() != x_s_sz)
               {
                   ret.setSize (x_s_sz);
               }

               ret.fill (value_type (0));

               value_type s = x_s [x_s_sz - 1];

               vector_type x (0);
               x_s.ref (x, 0, x_s_sz - 1);

               value_type fi_x = value_type (0);

               vector_type bufferGrad_x (0);
               if (this->myBufferGrad.getSize() != x_s_sz)
               {
                   this->myBufferGrad.setSize (x_s_sz);
               }
               this->myBufferGrad.ref (bufferGrad_x, 0, x_s_sz - 1);

               this->myBufferGrad [x_s_sz - 1] = -1;
               //= sum ( grad f_i(x) / (s - f_i(x)) )
               for (goSize_t i = 0; i < M; ++i)
               {
                   //== nabla phi(x) = sum_{i=1}^{M} 1/-f_i(x) \, nabla f_i(x)
                   //= 1 / f_i(x)
                   fi_x = value_type(1) / (s - (*this->myProblem->ineq(i))(x));
                   this->myProblem->ineq(i)->grad (x, bufferGrad_x);
                   bufferGrad_x *= -1.0; // FIXME  --- WHY IS THIS???
                   goMath::vectorAdd (fi_x, this->myBufferGrad, ret);
                   //printf ("Added to grad:\n");
                   //this->myBufferGrad.print ();
                   //char c;
                   //std::cin >> c;
               }

               //= f(x,s) = s, d/d(x,s) f(x,s) = (0,...,0,1)^T
               // this->myProblem->f()->grad (x, bufferGrad_x);
               // goMath::vectorAdd (this->my_t, myBufferGrad, ret);
               ret [x_s_sz - 1] += this->my_t * 1.0;

               printf ("BarrierOptFunctionPhase1::grad: \n");
               ret.print ();
           }

           virtual void hessian (vector_type& x_s, matrix_type& ret)
           {
               const goSize_t M = this->myProblem->ineqCount ();
               const goSize_t N = x_s.getSize ();

               if (this->myBufferGrad.getSize() != N)
               {
                   this->myBufferGrad.setSize (N);
               }

               if (ret.getColumns() != N || ret.getRows() != N)
               {
                   ret.resize (N, N);
               }

               if (this->myBufferHess.getRows () != N || this->myBufferHess.getColumns() != N)
               {
                   this->myBufferHess.resize (N, N);
               }

               {
                   vector_type v (0);
                   this->myBufferHess.refRow (N - 1, v);
                   v.fill (0.0);
                   this->myBufferHess.refColumn (N - 1, v);
                   v.fill (0.0);
               }

               this->myBufferGrad.fill (value_type (0));
               ret.fill (value_type (0));

               vector_type x (0);
               x_s.ref (x, 0, N - 1);

               value_type s = x_s [N - 1];

               vector_type bufferGrad_x (0);
               this->myBufferGrad.ref (bufferGrad_x, 0, N - 1);
               // myBufferGrad.fill (value_type(0));

               this->myBufferGrad [N - 1] = value_type (0);

               matrix_type bufferHess_x (0, 0);
               this->myBufferHess.ref (0, 0, N - 1, N - 1, bufferHess_x);

               //=
               //= Boyd eq. (11.14)
               //=

               value_type fi_x = value_type (0);

               vector_type ret_last_row (0), ret_last_col (0);
               ret.refRow (N - 1, 0, N - 1, ret_last_row); //= Only the x part, size N-1
               ret.refColumn (0, N - 1, N - 1, ret_last_col); //= Only the x part, size N-1

               for (goSize_t i = 0; i < M; ++i)
               {
                   //== nabla phi(x) = sum_{i=1}^{M} 1/-f_i(x) \, nabla f_i(x)
                   //= 1 / f_i(x)
                   fi_x = value_type(1) / (s - (*this->myProblem->ineq(i))(x));
                   this->myProblem->ineq(i)->grad (x, bufferGrad_x);

                   goMath::vectorAdd (-fi_x * fi_x, bufferGrad_x, ret_last_row);
                   goMath::vectorAdd (-fi_x * fi_x, bufferGrad_x, ret_last_col);
                   ret (N - 1, N - 1) += fi_x * fi_x;

                   //printf ("myBufferGrad:\n");
                   //this->myBufferGrad.print ();
                   //= sum 1/f_i(x)^2 nabla f_i(x) (nabla f_i(x))^\top
                   goMath::vectorOuter<value_type> (fi_x * fi_x, this->myBufferGrad, this->myBufferGrad, ret);

                   this->myProblem->ineq(i)->hessian (x, bufferHess_x);
                   //printf ("myBufferHess:\n");
                   //this->myBufferHess.print ();
                   this->myBufferHess *= fi_x;
                   ret += this->myBufferHess;
               }

               //= f(x,s) = s, Hess(f) = 0
               // myProblem->f()->hessian (x, myBufferHess);
               // myBufferHess *= my_t;
               // ret += myBufferHess;
               printf ("BarrierOptFunctionPhase1::hessian: \n");
               ret.print ();
           }

       private:
           const vector_type myVector;
   };
   #endif


    template <class matrix_type, class vector_type, class opt_function_type = BarrierOptFunction<matrix_type, vector_type> >
        class BarrierOpt
        {
            public:
                typedef typename matrix_type::value_type value_type;
                // typedef BarrierOptFunction<matrix_type, vector_type> opt_function_type;

            public:
                BarrierOpt (goAutoPtr<OptProblem<matrix_type, vector_type> > prob)
                    : myFunction (new opt_function_type),
                      myInfeasible (true),
                      myExtraStopCondition (0)
                {
                    myFunction->setProblem (prob);
                }

                virtual ~BarrierOpt ()
                {
                }

                /*! @brief Set the infeasible start point flag.
                 *
                 * @param i If true, the initial point is assumed to fulfill the inequality constraints,
                 * but not necessarily the equality constraints.
                 */
                void setInfeasible (bool i)
                {
                    myInfeasible = i;
                }

                /*! @brief Get the infeasible start point flag.
                 *
                 * @see setInfeasible()
                 *
                 * @return If true, the initial point must only fulfill the inequality constraints,
                 * but not necessarily the equality constraints.
                 */
                bool infeasible () const
                {
                    return myInfeasible;
                }

                void setExtraStopCondition (goAutoPtr<goFunctorBase0<bool> > f)
                {
                    myExtraStopCondition = f;
                }

                /** 
                * @brief Solve using the log barrier interior point method.
                * 
                * @todo Document choice of t0, mu, epsilon
                *
                * @param x Start point, must be \b strictly \b feasible. Contains the solution on return.
                */
                void solve (vector_type& x, value_type epsilon = 0.01, value_type mu = 2, value_type t0 = 1)
                {
                    //NewtonOpt <value_type> *newton = 0;
                    ////if (myFunction->problem()->eqA().isNull() && myFunction->problem()->eqB().isNull())
                    //{
                    //    newton = new NewtonOpt <value_type> (myFunction);
                    //}
                    //else
                    //{
                    //    newton = new NewtonOptEq <value_type> (myFunction, myFunction->problem()->eqA(), myFunction->problem()->eqB());
                    //    ((NewtonOptEq<value_type>*)newton)->setInfeasible (this->myInfeasible);
                    //}

                    NewtonOptEq <value_type> newton (myFunction, myFunction->problem()->eqA(), myFunction->problem()->eqB());

                    newton.setInfeasible (this->myInfeasible);

                    myFunction->setT (t0);
                    
                    goSize_t m = myFunction->problem()->ineqCount ();
                    if (myFunction->problem()->nonNegativity ())
                    {
                        m += x.getSize ();
                    }
                        
                    while (true)
                    {
                        newton.solveLineSearch (x);
                        printf ("t * f(x) + Barrier Function value %f\n", (*myFunction)(x));
                        printf ("At point:\n");
                        x.print ();

                        //for (goSize_t i = 0; i < m; ++i)
                        //{
                        //    printf ("Inequality %ld: %f\n", i, (*myFunction->problem()->ineq (i)) (x));
                        //}
                
                        printf ("BarrierOpt: t = %f\n", myFunction->t());

                        //= Check the extra stop condition, if any
                        if (!myExtraStopCondition.isNull() && (*myExtraStopCondition)())
                        {
                            break;
                        }

                        if (float(m) / myFunction->t() < epsilon)
                        {
                            break;
                        }

                        myFunction->setT (myFunction->t() * mu);
                    }
                }

                goAutoPtr<opt_function_type> function () 
                {
                    return this->myFunction;
                }

            private:
                goAutoPtr<opt_function_type>      myFunction;
                bool                              myInfeasible;
                goAutoPtr<goFunctorBase0<bool> >  myExtraStopCondition;
        };

static void fpTraps ()
    {
        ::feclearexcept (FE_ALL_EXCEPT);
#ifndef OSX
        ::fedisableexcept (FE_ALL_EXCEPT);
        ::feenableexcept (FE_DIVBYZERO | FE_INVALID);
#endif
        // ::feenableexcept (FE_DIVBYZERO | FE_UNDERFLOW | FE_INVALID);
        //::feenableexcept (FE_DIVBYZERO | FE_UNDERFLOW | FE_OVERFLOW | FE_INVALID);
        // ::fedisableexcept (FE_INEXACT);   // raised e.g. by sqrt(2)
        // signal (SIGFPE, (sighandler_t)fpe_handler);
    }

    template <class matrix_type, class vector_type, class opt_function_type = BarrierOptFunctionPhase1<matrix_type, vector_type> >
        class BarrierOptPhase1
        {
            public:
                typedef typename matrix_type::value_type value_type;

            public:
                BarrierOptPhase1 (goAutoPtr<OptProblem<matrix_type, vector_type> > prob)
                    : myFunction (new opt_function_type),
                      my_s (0)
                {
                    myFunction->setProblem (prob);
                }

                virtual ~BarrierOptPhase1 ()
                {
                }
               
                bool stopCondition ()
                {
                    printf ("Stop condition check: %f\n", *my_s);
                    return *my_s < value_type (0);
                }

                void solve (vector_type& x_s, value_type epsilon = 0.01, value_type mu = 2, value_type t0 = 1)
                {
                    const goSize_t N = x_s.getSize ();

                    fpTraps ();

                    //=
                    //= Find an x fulfilling AA x = bb
                    //=
                    const goSize_t m = myFunction->problem()->eqA()->getRows ();
                    const goSize_t n = myFunction->problem()->eqA()->getColumns ();
                    if (m != myFunction->problem()->eqB()->getSize())
                    {
                        goString s = "BarrierOptPhase1::solve(): eqB has wrong element count. Throwing exception.";
                        s += "\n  element count: ";
                        s += (int)myFunction->problem()->eqB()->getSize();
                        goLog::warning (s.toCharPtr());
                        throw goException();
                    }
                    goAutoPtr<matrix_type> A (new matrix_type (m, n + 1));
                    goAutoPtr<vector_type> b (new vector_type (m));

                    vector_type v (0);
                    A->refColumn (n, v);
                    v.fill (0.0);

                    matrix_type AA (0, 0);
                    A->ref (0, 0, m, n, AA);
                    AA = *myFunction->problem()->eqA();

                    myFunction->problem()->eqB()->copy (*b);
                    
                    vector_type x (0);
                    x_s.ref (x, 0, N - 1);
                    printf ("x size: %d\n", x.getSize ());

                    //= Solve AA x = b
                    if (false)
                    {
                        b->copy (x);
                        matrix_type AAA (AA);

                        if (!goMath::Lapack::gels (AAA, false, x))
                        {
                            goLog::warning ("BarrierOptPhase1::solve(): gels failed. Throwing exception.");
                            throw goException ();
                        }
                    }

                    printf ("feasible x:\n");
                    x.print ();
                    
                    //=
                    //= Find initial s so that all inequalities are met
                    //=
                    value_type s = value_type (-10);
                    goSize_t M = myFunction->problem()->ineqCount ();
                    for (goSize_t i = 0; i < M; ++i)
                    {
                        s = goMath::max<value_type> (s, (*myFunction->problem()->ineq (i)) (x));
                    }
                    s *= 1.5; //= inequalities must be strictly fulfilled --- therefore, add 10%

                    x_s [N - 1] = s;

                    printf ("x_s:\n");
                    x_s.print ();

                    printf ("A, b:\n");
                    A->print ();
                    b->print ();

                    // asm ("int $3"); // Breakpoint

                    //=
                    //= Create exactly the same problem like the original, just change the equality constraints A, b so that they fit
                    //= x_s.
                    //=
                    goAutoPtr<OptProblem <matrix_type, vector_type> > phase1Problem = 
                        new OptProblem <matrix_type, vector_type> (*myFunction->problem());
                    phase1Problem->setEqCon (A, b);

                    //= Solve the problem min s  s.t. f_i <= s and A x = b with the barrier method
                    // goAutoPtr<BarrierOptFunctionPhase1 <vector_type, matrix_type> > bop1 = new  BarrierOptFunctionPhase1 <vector_type, matrix_type> (myFunction);
                    BarrierOpt <matrix_type, vector_type, opt_function_type> bo (phase1Problem);  // myFunction->problem());
                    // bo.setInfeasible (false);
                    my_s = &x_s[x_s.getSize() - 1];
                    // bo.setExtraStopCondition (goMemberFunction <BarrierOptPhase1 <matrix_type, vector_type, opt_function_type>, bool> (this, &BarrierOptPhase1 <matrix_type, vector_type, opt_function_type>::stopCondition));
                    bo.solve (x_s, epsilon, mu, t0);

                    printf ("phase 1 x_s:\n");
                    x_s.print ();
                }

            private:
                goAutoPtr<opt_function_type> myFunction;

                value_type *my_s;  //= Used in the extra stop condition
        };

};

#endif
