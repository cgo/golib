#include <gobarrieropt.h>
#include <goplot.h>
#include <gorandom.h>

using namespace goMath;

typedef Matrix<goFloat> matrix;
typedef Vector<goFloat> vector;

class SVMDual : public OptFunction <matrix, vector>
{
    public:
        typedef matrix::value_type value_type;

    public:
        SVMDual (goAutoPtr<matrix> innerProd, goAutoPtr<vector> y, double eps = 0.01)
            : OptFunction <matrix, vector> (eps),
               my_innerProducts (innerProd),
               my_y (y)
        {
        }

        value_type operator () (const vector& alpha)
        {
            goSize_t sz = alpha.getSize();
            value_type sum = 0;
            value_type sum2 = 0;
            for (goSize_t i = 0; i < sz; ++i)
            {
                sum += alpha[i];
                for (goSize_t j = i + 1; j < sz; ++j)
                {
                    sum2 += alpha[i] * alpha[j] * (*my_y)[i] * (*my_y)[j] * (*my_innerProducts) (i,j);
                }
                sum2 += 0.5 * alpha[i] * alpha[i] * (*my_y)[i] * (*my_y)[i] * (*my_innerProducts) (i,i);
            }

            return sum2 - sum; // Times -1 because we minimise by default
        }

        
    private:
        goAutoPtr<matrix> my_innerProducts;  //= Inner products
        goAutoPtr<vector> my_y;              //= labels
};

int main ()
{
    const int N = 10;
    matrix points (2*N, 2);
    matrix::vector_iterator it = points.rowBegin ();
    goAutoPtr<vector> y = new vector (2*N);
    for (int i = 0; i < N; ++it, ++i)
    {
        (*it)[0] = 0.5 * goRandom() + 0.5;
        (*it)[1] = 0.5 * goRandom() + 0.9;
        (*y)[i] = 1.0;
    }
    for (int i = 0; i < N; ++it, ++i)
    {
        (*it)[0] = 0.5 * goRandom() + 1.0;
        (*it)[1] = 0.5 * goRandom() + 1.5;
        (*y)[N + i] = -1.0;
    }

    goPlot::Plot p;
    matrix pointsref (points.getPtr(), N, 2);
    p.plot (pointsref, "", "w p lc rgb \"blue\"");
    pointsref.setData (points.getPtr() + 2 * N, N, 2);
    p.plot (pointsref, "", "w p lc rgb \"red\"");
    p.plotPause ();

    goAutoPtr<matrix> xx = new matrix (2*N, 2*N);
    int i = 0, j = 0;
    for (it = points.rowBegin(); it != points.rowEnd(); ++it, ++i)
    {
        j = i;
        for (matrix::vector_iterator it2 = it; it2 != points.rowEnd(); ++it2, ++j)
        {
            (*xx) (i,j) = *it * *it2;
            (*xx) (j,i) = (*xx) (i,j);
        }
    }

    points *= 10.0;

    goAutoPtr<OptProblem<Matrix<goFloat>, Vector<goFloat> > > prob = new OptProblem<Matrix<goFloat>, Vector<goFloat> > (new SVMDual (xx, y));

    goAutoPtr<matrix> A = new matrix (1, y->getSize());
    A->setRow (0, *y);

    goAutoPtr<vector> b = new vector (1);
    (*b)[0] = 0;

    prob->setEqCon (A, b);
    prob->setNonNegativity (true);

//    vector alpha_s (2*N + 1);
//    alpha_s.fill (1.0);
//    BarrierOptPhase1<matrix, vector> phase1 (prob);
//    phase1.solve (alpha_s);

    vector alpha (2*N);
    alpha.fill (1.0);

    double epsilon = 0.001;
    BarrierOpt<Matrix<goFloat>, Vector<goFloat> > bo (prob);
    bo.solve (alpha, epsilon, 1.5, 1.0);

    alpha.print ();

    vector w (2);
    w.fill (0.0);
    i = 0;
    for (it = points.rowBegin(); it != points.rowEnd(); ++it, ++i)
    {
        w += *it * alpha[i] * (*y)[i];
    }

    w.print ();

    goIndex_t max_i = goMath::maxIndex (alpha);
    if (alpha[max_i] <= epsilon)
    {
        printf ("Max. alpha is <= epsilon --- no support vector found??\n");
    }

    printf ("max alpha: %f\n", alpha[max_i]);

    vector x_i (0);
    points.refRow (max_i, x_i);
    double bb = 1.0 / (*y)[max_i] - (w * x_i);

    // Aufpunkt --- stimmt das??
    vector z = (w / w.norm2()) * (-bb);

    vector line = w;
    line[0] = -w[1];
    line[1] = w[0];

    p.plotLine (line * 3, z);
    p.plotPause ();

    printf ("alpha * y (should be zero): %f\n", alpha * *y);

    //= Now classify the training data:
    printf ("Class 1: \n");
    it = points.rowBegin ();
    for (goSize_t i = 0; i < N; ++i, ++it)
    {
        printf ("%f\n", w * *it + bb);
    }
    printf ("Class -1: \n");
    for (goSize_t i = 0; i < N; ++i, ++it)
    {
        printf ("%f\n", w * *it + bb);
    }

    exit (1);
}
