/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GONEWTON_H
#define GONEWTON_H

#ifndef GOOPT_H
# include <goopt.h>
#endif
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
    // template <class callable_, class type_> class NewtonOptPrivate;

/** \addtogroup mathopt
 * @{
 */

    /** 
     * @brief Newton optimisation.
     *
     * @todo Add line search
     *
     * This implements the standard Newton step descent algorithm, solving
     * \f[ H(f(x)) \cdot \Delta x = \nabla f(x) \f]
     * to find a step \f$\Delta x\f$ and then updating
     * \f[ x = x - \Delta x \f]
     * until convergence.
     * A line search is not conducted, this needs to be added.
     *
     * @param callable_ A type that supports the interface defined by the class
     * goMath::OptFunction, with goMath::Matrix and goMath::Vector
     * types as template parameters. You can simply derive from OptFunction 
     * and implement the \c operator() accordingly, or use a OptFunctor.
     * The function to be minimised will be of type \c callable_.
     * @param type_ A floating point type (float or double).
     */
    template <class type_>
    class NewtonOpt
    {
        public:
            typedef goMath::Vector<type_> vector_type;
            typedef goMath::Matrix<type_> matrix_type;

            typedef goMath::OptFunction<matrix_type, vector_type> function_type;

        public:
            /** 
             * @brief Constructor.
             * 
             * @note \c eps is used squared in hessian(), therefore it should not be much smaller than 0.01 for floats
             * in order not to run into machine precision problems.
             *
             * @param f Function object that is to be minimised.
             */
            NewtonOpt (goAutoPtr<function_type> f)
                : myF (f),
                  myHessian (0, 0),
                  myLineSearch (1, 0.2, 0.5)
            { }
            virtual ~NewtonOpt () { }

            /** 
             * @brief Sets the function to be minimised.
             * 
             * @param f Function object that is to be minimised.
             */
            void setF (goAutoPtr<function_type> f)
            {
                myF = f;
            }

            /** 
             * @brief Frees temporary memory.
             */
            virtual void cleanup ()
            {
                myHessian.resize (0, 0);
            }

            /** 
             * @brief Solves without line search.
             * 
             * @param x Initial point.
             */
            virtual void solveDirect (vector_type& x)
            {
                const type_ epsilon = type_ (1e-5);
                type_ stopping = type_ (1);

                vector_type dx (x.getSize ());

                while (stopping > epsilon)
                {
                    this->step (x, dx);
                    printf ("f == %f\n", (*myF)(x));
                    x -= dx;
                    stopping = dx * dx;
                }

                this->cleanup ();
            }

            virtual void solveLineSearch (vector_type& x)
            {
                const type_ epsilon = type_ (1e-5);
                type_ stopping = type_ (1);

                vector_type dx (x.getSize ());

                type_ t = type_ (1);
                type_ fx = type_ (0);
                vector_type nabla_fx;
                
                while (stopping > epsilon && t > 1e-6)
                {
                    this->step (x, dx);
                    
                    fx = (*myF)(x);
                    myF->grad (x, nabla_fx);
                    t = this->myLineSearch (*myF, fx, nabla_fx, x, dx);
                    printf ("t == %.20f\n", t);

                    printf ("f == %f\n", fx);
                    stopping = dx * dx;
                    dx *= t;
                    x -= dx;
                }

                this->cleanup ();
            }


             /* @brief Calculates the Newton decrement at x.
              *
              * Calculates \f[ x^\top \nabla f(x) \f]
              *
             * Calculating this during the minimisation, if needed, will be more efficient than calling
             * this method.
             * Can be used as a stopping criterion.
             *
             * @param x Point at which to calculate the decrement.
             * 
             * @return 
             */
            virtual type_ newtonDecrement (vector_type& x)
            {
                vector_type g (x.getSize());

                this->myF->grad (x, g);
                return x * g;
            }

            /** 
             * @brief Calculate the newton step.
             *
             * @note x is modified during the call, but restored when the call returns.
             *
             * @param x Point at which to calculate
             * @param ret Contains the Newton step on return
             */
            virtual void step (vector_type& x, vector_type& ret)
            {
                goSize_t sz = x.getSize ();
                //= Hessian is symmetric; 

                if (ret.getSize() != sz)
                {
                    ret.setSize (sz);
                }

                this->myF->hessian (x, myHessian);
                this->myF->grad (x, ret);
                if (!goMath::Lapack::posv (myHessian, ret))
                {
                    printf ("******************* posv FAILED! ********************\n");
                }
            }

        protected:
            // double myEpsilon;
            goAutoPtr<function_type> myF;

            matrix_type myHessian; //= For step calculation -- call cleanup to free memory.

            LineSearch<function_type, vector_type> myLineSearch;
    };

    /** 
     * @brief Newton optimisation with linear equality constraints.
     * @see NewtonOpt
     *
     * @todo Detect when Ax=b is fulfilled; then, calculating Ax-b in each step can be skipped (is always feasible then).
     *
     * @par References
     * Boyd, S. & Vandenberghe, L.: Convex Optimization. Cambridge University Press, 2004
     *
     * This is the infeasible start Newton method to solve
     * \f[ \min f(x) \text{ subject to } A\, x = b \, . \f]
     *
     * The Newton step is here calculated by solving
     * \f[ \left(\begin{array}{c c} H(F) & A^\top \\ A & 0 \end{array}\right) \cdot \left( \begin{array}{c} \Delta x \\w \end{array} \right) = \left( \begin{array}{c} \nabla F(x) \\A \, x - b \end{array} \right) \f]
     * with H(F) the Hessian matrix of F.
     */
    template <class type_>
    class NewtonOptEq : public NewtonOpt <type_>
    {
        public:
            typedef goMath::Vector<type_> vector_type;
            typedef goMath::Matrix<type_> matrix_type;

            typedef goMath::OptFunction<matrix_type, vector_type> function_type;

        public:
            /** 
             * @brief Constructor.
             *
             * Construct a Newton optimisation object with equality constraints
             * \f[ A \, x = b \, .\f]
             * 
             * @param f Function object to be minimised
             * @param A  Matrix A
             * @param b  Right hand side vector b
             * @param eps Epsilon, see NewtonOpt
             */
            NewtonOptEq (goAutoPtr<function_type> f, goAutoPtr<matrix_type> A = 0, goAutoPtr<vector_type> b = 0)
                : NewtonOpt <type_> (f),
                  myA (A),
                  myB (b),
                  myKKT_A (0, 0),
                  myKKT_x (0),
                  myKKT_b (0),
                  myInfeasible (true)
            {
            }

            virtual ~NewtonOptEq ()
            {
            }

            /** 
             * @brief Set the equality constraints \f$A \, x = b \f$
             * 
             * @param A Matrix A
             * @param b Vector b
             */
            void setEq (goAutoPtr<matrix_type> A, goAutoPtr<vector_type> b)
            {
                myA = A;
                myB = b;
            }

            //= No line search
            /** 
             * @brief Solve without line search.
             *
             * Solves directly without line search.
             * The initial point may be infeasible.
             * 
             * @param x Initial point
             */
            virtual void solveDirect (vector_type& x)
            {
                if (this->myA.isNull () || this->myB.isNull())
                {
                    goLog::warning ("NewtonOptEq::solveDirect(): A or b is null. Not solving.");
                    return;
                }
                this->initKKT (x.getSize());
                NewtonOpt<type_>::solveDirect (x);
            }

            virtual void solveLineSearch (vector_type& x)
            {
                if (this->myA.isNull () || this->myB.isNull())
                {
                    goLog::warning ("NewtonOptEq::solveDirect(): A or b is null. Not solving.");
                    return;
                }
                this->initKKT (x.getSize());
                NewtonOpt<type_>::solveLineSearch (x);
            }

            /** 
             * @brief Frees temporary memory.
             */
            virtual void cleanup ()
            {
                NewtonOpt<type_>::cleanup ();
                myKKT_A.resize (0, 0);
                myKKT_b.setSize (0);
            }

            /** 
             * @brief Initialise the KKT system.
             * 
             * sets the equality constraint matrix A in 
             * the matrix \f[ KKT_A = \left(\begin{array}{c c} H(F) & A^\top \\ A & 0 \end{array}\right) \f].
             * The hessian H(F) must be filled by the step() method.
             * If necessary, resizes the right hand side vector \f[ KKT_b \f] of the
             * system \f[ KKT_A \cdot \left( \begin{array}{c} \Delta x \\w \end{array} \right) = \left( \begin{array}{c} \nabla F(x) \\A \, x - b \end{array} \right) \f]
             *
             * @param sz Dimension of x.
             */
            void initKKT (goSize_t sz)
            {
                goSize_t KKT_r = sz + myA->getRows();
                goSize_t KKT_c = KKT_r;

                if (myKKT_b.getSize() != KKT_r)
                {
                    myKKT_b.setSize (KKT_r);
                }

                if (myKKT_A.getRows() != KKT_r || myKKT_A.getColumns() != KKT_c)
                {
                    myKKT_A.resize (KKT_r, KKT_c);
                }

                //= Fill lower right part of the KKT matrix with 0
                matrix_type temp (0, 0);
                myKKT_A.ref (sz, sz, KKT_r - sz, KKT_c - sz, temp);
                temp.fill (type_(0));

                {
                    matrix_type temp;
                    goSize_t r = myA->getRows ();
                    goSize_t c = myA->getColumns ();
                    this->myKKT_A.ref (0, sz, c, r, temp);
                    myA->copy (0, 0, r - 1, c - 1, temp, true);  //= Copy transposed to upper right
                    this->myKKT_A.ref (sz, 0, r, c, temp);
                    myA->copy (0, 0, r - 1, c - 1, temp, false); //= Copy non-transposed to lower left 
                }
            }


            //= x is modified during the call, but the same when the call returns.
            //= Boyd: 10.3.1, Eq. (10.19)
            virtual void step (vector_type& x, vector_type& ret)
            {
                goSize_t sz = x.getSize ();
                //= Hessian is symmetric; 

                if (ret.getSize() != sz)
                {
                    ret.setSize (sz);
                }

                //= getrf / getrs further below overwrite the myKKT_A, therefore we need to
                //= initialise at each step.
                this->initKKT (sz);

                //= Set second part of RHS vector b to A x - b
                {
                    //myKKT_b.fill (0.0);
                    vector_type temp;
                    myKKT_b.ref (temp, sz, myA->getRows());
                   
                    if (this->myInfeasible)
                    {
                        temp = *myB;
                        goMath::matrixVectorMult (type_(1), *myA, false, x, type_(-1), temp);
                        // temp = *myA * x - *myB;
                    }
                    else
                    {
                        temp.fill (0.0);
                    }
                }
                
                //= Upper left: Hessian (F)
                this->myKKT_A.ref (0, 0, sz, sz, this->myHessian); 

                vector_type g (0);
                //= Will hold grad F -- the rest is for Ax - b
                myKKT_b.ref (g, 0, sz); 

                this->myF->hessian (x, this->myHessian);
                this->myF->grad (x, g);

                printf ("NewtonOptEq::step(): KKT_A:\n");
                this->myKKT_A.print ();
                printf ("NewtonOptEq::step(): KKT_b:\n");
                this->myKKT_b.print ();

                //myKKT_A.print ();
                //myKKT_b.print ();

                //= Solve myKKT_A * [delta_x w]' = myKKT_b
#if 0
                goVector<int> piv;
                if (!goMath::Lapack::getrf (myKKT_A, piv))
                {
                    printf ("****************** getrf failed! ******************\n");
                    printf ("myKKT_A:\n");
                    myKKT_A.print ();
                }
                matrix_type B (myKKT_b.getPtr(), 1, myKKT_b.getSize());
                if (!goMath::Lapack::getrs (myKKT_A, false, B, piv))
                {
                    printf ("******************* getrs FAILED! ********************\n");
                }
#endif
                if (!goMath::Lapack::gels (myKKT_A, false, myKKT_b))
                {
                    printf ("********************* gels FAILED! ********************\n");
                }
                //if (!goMath::Lapack::posv (myKKT_A, myKKT_b))
                //{
                //    printf ("******************* posv FAILED! ********************\n");
                //}

                ret = g;

                printf ("NewtonOptEq::step(): \n");
                ret.print ();
            }
           
            /*! @brief Set infeasibile start point flag.
             *
             * @param i If true, the initial point is assumed infeasible.
             */
            void setInfeasible (bool i)
            {
                myInfeasible = i;
            }

            /*! @brief Returns the infeasibile start point flag.
             *
             * @see setInfeasible()
             *
             * If true, the initial point may be infeasible.
             */
            bool infeasible () const
            {
                return myInfeasible;
            }

        protected:
            goAutoPtr<matrix_type> myA;  //= Linear equality constraints: Ax = b
            goAutoPtr<vector_type> myB;

            matrix_type myKKT_A; //= KKT system: _A * _x = _b
            vector_type myKKT_x;
            vector_type myKKT_b;

            bool myInfeasible;  //= If true, the initial point is assumed infeasible.
    };
/** @} */

};
#endif
