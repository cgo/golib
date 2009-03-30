#ifndef GOBARRIEROPT_H
#define GOBARRIEROPT_H

#ifndef GOOPT_H
# include <goopt.h>
#endif
#ifndef GONEWTON_H
# include <gonewton.h>
#endif

namespace goMath
{
   //= Idee: BarrierOpt -> ruft NewtonOptEq mit BarrierOptFunction als 
   //= OptFunction (callable_) auf.
   //= Die Auessere Schleife laeuft in BarrierOpt.    

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

           void setProblem (goAutoPtr<OptProblem <matrix_type, vector_type> > f)
           {
               myProblem = f;
           }

           virtual value_type operator () (const vector_type& x) 
           {
               return (*(myProblem->f()))(x);
           }

           virtual void grad (vector_type& x, vector_type& ret)
           {
               const goSize_t M = myProblem->ineqCount ();

               if (ret.getSize() != M)
               {
                   ret.setSize (M);
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

               myProblem->f()->grad (x, myBufferGrad);
               goMath::vectorAdd (my_t, myBufferGrad, ret);
           }

           virtual void hessian (vector_type& x, matrix_type& ret)
           {
               const goSize_t M = myProblem->ineqCount ();

               if (myBufferGrad.getSize() != M)
               {
                   myBufferGrad.setSize (M);
               }

               if (ret.getColumns() != M || ret.getRows() != M)
               {
                   ret.resize (M, M);
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
               myProblem->f()->hessian (x, myBufferHess);
               myBufferHess *= my_t;
               ret += myBufferHess;
           }

           //= Calculate gradient and Hessian of the logarithmic barrier function
           void barrierGradHessian (const vector_type& x, vector_type& grad, matrix_type& hess)
           {
               const goSize_t M = myProblem->ineqCount ();

               if (grad.getSize() != M)
               {
                   grad.setSize (M);
               }

               if (hess.getColumns() != M || hess.getRows() != M)
               {
                   hess.resize (M, M);
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

       private:
           goAutoPtr<OptProblem<matrix_type, vector_type> > myProblem;
           vector_type myBufferGrad;
           matrix_type myBufferHess;

           value_type my_t;
   };

    template <class matrix_type, class vector_type>
        class BarrierOpt
        {
            public:
                typedef typename matrix_type::value_type value_type;
                typedef BarrierOptFunction<matrix_type, vector_type> opt_function_type;

            public:
                BarrierOpt (goAutoPtr<OptProblem<matrix_type, vector_type> > prob)
                    : myFunction (new BarrierOptFunction<matrix_type, vector_type>)
                {
                    myFunction->setProblem (prob);
                }

                virtual ~BarrierOpt ()
                {
                }

                void solveDirect (vector_type& x)
                {
                    NewtonOptEq <value_type> newton (myFunction->problem()->f(), myFunction->problem()->eqA(), myFunction->problem()->eqB());

                    myFunction->setT (value_type (1));
                    for (goSize_t n = 0; n < 10; ++n)
                    {
                        newton.solveDirect (x);
                        printf ("Function value %f\n", (*myFunction)(x));
                        printf ("At point:\n");
                        x.print ();

                        goSize_t cnt = myFunction->problem()->ineqCount ();
                        for (goSize_t i = 0; i < cnt; ++i)
                        {
                            printf ("Inequality %d: %f\n", i, (*myFunction->problem()->ineq (i)) (x));
                        }
                        myFunction->setT (myFunction->t() * 2.0);
                    }
                }

            private:
                goAutoPtr<opt_function_type> myFunction;
        };
};

#endif
