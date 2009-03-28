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
    template <class callable_, class matrix_type, class vector_type>
        class BarrierOptFunction : public OptFunction <matrix_type, vector_type>
    {
        public:
            typedef OptFunction<matrix_type,vector_type>::value_type value_type;

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

            void setProblem (goAutoPtr<OptFunction<callable_, matrix_type, vector_type> > f)
            {
                myProblem = f;
            }

            virtual value_type operator () (const vector_type& x)
            {
                return (*myProblem->f())(x);
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

        private:
            goAutoPtr<OptProblem<callable_, matrix_type, vector_type> > myProblem;
            vector_type myBufferGrad;
            matrix_type myBufferHess;

            value_type my_t;
    };

    template <class callable_, class matrix_type, class vector_type>
        class BarrierOpt
        {
            public:
                typedef matrix_type::value_type value_type;

            public:
                BarrierOpt (goAutoPtr<OptProblem<callable_, matrix_type, vector_type> > prob)
                    : myFunction (new BarrierOptFunction<callable_, matrix_type, vector_type>)
                {
                    myFunction->setProblem (prob);
                }

                virtual ~BarrierOpt ()
                {
                }

            private:
                goAutoPtr<BarrierOptFunction<callable_, matrix_type, vector_type> > myFunction;
        };
};

#endif
