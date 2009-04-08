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

       private:
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
           typedef OptFunction<matrix_type, vector_type> parent;
           typedef matrix_type::value_type value_type;

           BarrierOptFunctionPhase1 (goAutoPtr<parent> p, double eps = 0.01)
               : OptFunction <matrix_type, vector_type> (eps),
                 myOriginalFunction (p),
                 myVector (0),
                 myS (s)
           {
           }

           virtual ~BarrierOptFunctionPhase1 ()
           {
           }

           virtual value_type operator () (const vector_type& x)
           {
               const_cast<vector_type*> (&myVector) -> setData (const_cast<valut_type*> (x.getPtr()), x.getSize() - 1, x.getStride ());
               return (*myOriginalFunction) (myVector) - x[x.getSize() - 1];
           }

           //= Last entry of x is s
           virtual void grad (vector_type& x_s, vector_type& ret)
           {
               const goSize_t M = myProblem->ineqCount ();

               if (ret.getSize() != x_s.getSize())
               {
                   ret.setSize (x_s.getSize());
               }

               ret.fill (value_type (0));

               value_type s = x_s [x_s.getSize()];

               vector_type x (0);
               x_s.ref (x, 0, x_s.getSize() - 1);

               value_type fi_x = value_type (0);
               for (goSize_t i = 0; i < M; ++i)
               {
                   //== nabla phi(x) = sum_{i=1}^{M} 1/-f_i(x) \, nabla f_i(x)
                   //= 1 / f_i(x)
                   fi_x = value_type(1) / ((*myProblem->ineq(i))(x) + s);
                   myProblem->ineq(i)->grad (x, myBufferGrad);
                   goMath::vectorAdd (-fi_x, myBufferGrad, ret);
               }

               myProblem->f()->grad (x, myBufferGrad);
               goMath::vectorAdd (my_t, myBufferGrad, ret);
           }

       private:
           goAutoPtr<parent> myOriginalFunction;
           const vector_type myVector;
           value_type myS;
   };
   #endif

   #if 0
   template <class matrix_type, class vector_type>
       class BarrierOptPhase1
       {
           public:
               typedef typename matrix_type::value_type value_type;

               BarrierOptPhase1 ()
                   : myS (0),
                     myOriginalProblem (0)
               {
               }

               virtual ~BarrierOptPhase1 ()
               {
               }

               void phase1Basic (vector_type& x)
               {
                   goSize_t cnt = myOriginalProblem->problem()->ineqCount ();
                   value_type s = value_type (0);

                   //= Solve Ax = b to find a feasible x_0
                   {
                       goMath::Vector<int> pivot;
                       matrix_type M (*myFunction->problem()->eqA());
                       goMath::Lapack::getrf (M, pivot);
                       x = *myFunction->problem()->eqB ();
                       matrix_type B (x.getPtr(), 1, xgetSize ());
                       goMath::Lapack::getrs (M, false, B);
                   }

                   //= Make the modified inequality conditions f_i(x) < s feasible by choosing s.
                   for (goSize_t i = 0; i < cnt; ++i)
                   {
                       s = goMath::max (s, (*myFunction->problem()->ineq (i)) (x));
                   }

                   myS = s * value_type (1.2);

                   //= Create problem with modified inequality constraints
                   //= FIXME: MAke a whole new problem -- min_{x,s} s wrt f_i(x) <= s, Ax = b.
                   goAutoPtr<OptProblem<matrix_type, vector_type> > newProblem = new OptProblem<matrix_type,vector_type> ( FIXME )
                   newProblem->setEqCon (myOriginalProblem->eqA(), myOriginalProblem->eqB());

                   for (goSize_t i = 0; i < cnt; ++i)
                   {
                        newProblem->addIneqCon (new OptFunctionAdd<matrix_type,vector_type> (myOriginalProblem->ineq (i)));
                   }
               }

               value_type f (const vector_type&)
               {
                   
               }

           private:
               value_type myS;
               goAutoPtr<OptProblem<matrix_type, vector_type> > myOriginalProblem;
       };
#endif

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

                void solve (vector_type& x)
                {
                    NewtonOptEq <value_type> newton (myFunction->problem()->f(), myFunction->problem()->eqA(), myFunction->problem()->eqB());

                    myFunction->setT (value_type (1));
                    for (goSize_t n = 0; n < 3; ++n)
                    {
                        newton.solveLineSearch (x);
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

                goAutoPtr<opt_function_type> function () 
                {
                    return this->myFunction;
                }

            private:
                goAutoPtr<opt_function_type> myFunction;
        };
};

#endif
