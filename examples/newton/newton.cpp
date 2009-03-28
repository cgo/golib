#include <gotypes.h>
#include <gomath.h>
#include <govector.h>
#include <gomatrix.h>
#include <gonewton.h>
#include <goopt.h>
#include <gobarrieropt.h>
#include <goplot.h>
#include <gofunctor.h>

typedef goFloat REAL;
typedef goVector<REAL> vector_type;
typedef goMatrix<REAL> matrix_type;

REAL ff (const vector_type& x)
{
    return ( (x * x) ) * 0.5 + 4;
}

    class Function : public goMath::OptFunction <matrix_type, vector_type>
    {
        public:
            Function () 
                : goMath::OptFunction <matrix_type, vector_type> () { };
            ~Function () { };

            virtual REAL operator () (const vector_type& x) const
            {
                // return ((x * x) * (x * x)) * 0.5 + x * x;
                return ( (x * x) ) * 0.5 + 4;
            }
    };

    class Function2D : public goMath::OptFunction <matrix_type, vector_type>
    {
        public:
            Function2D ()
                : goMath::OptFunction <matrix_type, vector_type> (),
                  A (2, 2),
                  c (2)
                { 
                    REAL A_[] = {2, 0,
                                 0, 1};
                    this->A = matrix_type (A_, 2, 2);
                    c[0] = 1;
                    c[1] = 1;
                }
            ~Function2D () { }
            
            virtual REAL operator () (const vector_type& x) const
            {
                return x * (A * x) + c * x;
            }

            matrix_type A;
            vector_type c;
    };

//= Inequality constraints
REAL f1 (const vector_type& x)
{
    // |x| >= 1
    return REAL(1) - ::sqrt (x * x);
}

int main ()
{

    //= Do log barrier interior point
    {
        goAutoPtr<Function2D> f (new Function2D);
        goAutoPtr<goMath::OptProblem <goMath::OptFunction<matrix_type, vector_type>, matrix_type, vector_type> > problem (new goMath::OptProblem <goMath::OptFunction<matrix_type, vector_type>, matrix_type, vector_type> (f));
        problem->addIneqCon (new goMath::OptFunctor<matrix_type, vector_type> (goFunction <REAL, const vector_type&> (f1)));

        {
            matrix_type* A = new matrix_type;
            A->resize (1, 2);
            REAL A_[] = { 1.0, 1.0 };
            *A = matrix_type (A_, 1, 2);
            vector_type* b = new vector_type (1);
            (*b)[0] = 1.0;

            problem->setEqCon (A, b);
        }

        goMath::BarrierOpt <goMath::OptFunction<matrix_type, vector_type>, matrix_type, vector_type> bo (problem);

        vector_type x (2);
        x[0] = 10.0;
        x[1] = 10.0;

        bo.solveDirect (x);

        exit (1);
    }

//= Do 2D Newton
    {
        Function2D f;
        goMath::NewtonOpt <Function2D, REAL> newton (f);

        vector_type x (2);
        x[0] = 10.0;
        x[1] = 10.0;
        
        newton.solveDirect (x);

        vector_type xx (2);
        int N = 10;
        REAL x1 = -2;
        REAL x2 = 2;
        matrix_type M (N, N);
        matrix_type points (N * N, 3);
        for (int i = 0; i < N; ++i)
        {
            xx[0] = x1 + (x2 - x1) * i / float (N - 1);
            for (int j = 0; j < N; ++j)
            {
                xx[1] = x1 + ( x2 - x1 ) * j / float (N - 1);
                M (i, j) = f (xx);
                points (i * N + j, 0) = xx[0];
                points (i * N + j, 1) = xx[1];
                points (i * N + j, 2) = f (xx);
            }
        }

        printf ("Solution:\n");
        x.print ();

        goSinglePlot sp;
        // sp.add3D (M, "");
        {
            vector_type x, y, v;
            points.refColumn (0, x);
            points.refColumn (1, y);
            points.refColumn (2, v);
            sp.add3D (x, y, N, v, "");
        }


        //= Do equality constrained Newton
        {
            matrix_type* A = new matrix_type;
            A->resize (1, 2);
            REAL A_[] = { 1.0, 1.0 };
            *A = matrix_type (A_, 1, 2);
            vector_type* b = new vector_type (1);
            (*b)[0] = 1.0;

            x[0] = 10.5;
            x[1] = 10.5;
            printf ("Ax - b = ");
            (*A * x - *b).print ();
            goMath::NewtonOptEq <Function2D, REAL> newton (f, A, b);
            // newton.setEq (A, b);
            newton.solveDirect (x);

            printf ("Ax - b = ");
            (*A * x - *b).print ();

            printf ("Solution: ");
            x.print ();

            vector_type point (3);
            point[0] = x[0]; point[1] = x[1]; point[2] = f (x);
            sp.addPoint (point, "", "w p ps 4 lw 3");
        }
        goMultiPlotter mp (1,1);
        mp.setPrefix ("set dgrid3d; set xrange [-2:2]; set yrange [-2:2]; set cntrparam levels discrete 1; set contour\n");
        sp.add3D ("x + y");
        mp.addPlot (sp, 0);
        mp.saveGnuplot ("2d");
        mp.setPauseFlag (true);
        mp.plot ();

        // exit (1);
    }


//= Do 1D Newton, and try the functor interface.
    // Function f;
    // goMath::NewtonOpt <Function, REAL> newton (f);
    goMath::OptFunctor <matrix_type, vector_type> f (new goFunction1 <REAL, const vector_type&> (ff));
    goMath::NewtonOpt <goMath::OptFunctor<matrix_type, vector_type>, REAL> newton (f);

    vector_type g;
    vector_type x (1);
    x[0] = 30;
    f.grad (x, g);
    
    std::cout << "grad (" << x[0] << "): ";
    g.print ();

    matrix_type H;
    f.hessian (x, H);
    std::cout << "Hessian:";
    H.print ();

    newton.solveDirect (x);

    // vector_type dx (1);
    //for (int i = 0; i < 10; ++i)
    //{
    //    newton.step (x, dx);
    //    printf ("%d: %f at %f, dx == %f\n", i, f(x), x[0], dx[0]);
    //    x -= dx;
   // }


    {
        goSize_t sz = 100;
        REAL x0 = -1;
        REAL x1 = 1;
        goVectorf X (sz);
        X.fillRange (x0, (x1 - x0) / float (sz - 1), x1 + 1);
        printf ("Size: %d\n", X.getReserved());
        goVectorf fx (sz);
        goVectorf dfx (sz);
        goVectorf ddfx (sz);
        printf ("Size: %d\n", fx.getReserved());
        for (goSize_t i = 0; i < sz; ++i)
        {
            x[0] = X[i];
            fx[i] = f(x);
            f.grad (x, g);
            dfx[i] = g[0];
            f.hessian (x, H);
            ddfx[i] = H (0, 0);
        }
        goGnuplot gp ("/usr/bin/gnuplot");
        goSinglePlot P;
        P.addCurve (X, fx, "f");
        P.addCurve (X, dfx, "grad f");
        P.addCurve (X, ddfx, "Hess f");
        goMultiPlotter mp (1,1);
        mp.addPlot (P,0);
        mp.saveGnuplot ("gnuplot.txt");
    }

    return 1;
}
