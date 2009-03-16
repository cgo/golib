#include <gotypes.h>
#include <gomath.h>
#include <govector.h>
#include <gomatrix.h>
#include <gonewton.h>
#include <goplot.h>

typedef goFloat real;
typedef goVector<real> vector_type;
typedef goMatrix<real> matrix_type;

    class Function
    {
        public:
            Function () { };
            ~Function () { };

            real operator () (const vector_type& x)
            {
                return ((x * x) * (x * x)) * 0.5 + x * x;
            }
    };

int main ()
{

    goMath::NewtonOpt <Function, real> newton (0.01);

    Function f;

    vector_type g;
    vector_type x (1);
    x[0] = 3;
    newton.grad (x, g);
    
    std::cout << "grad (" << x[0] << "): ";
    g.print ();

    matrix_type H;
    newton.hessian (x, H);
    std::cout << "Hessian:";
    H.print ();

    {
        goSize_t sz = 100;
        real x0 = -1;
        real x1 = 1;
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
            newton.grad (x, g);
            dfx[i] = g[0];
            newton.hessian (x, H);
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
