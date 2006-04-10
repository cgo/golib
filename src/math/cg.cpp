#include <gosparsematrix.h>
#include <govector.h>

/*
 * Schaback, Numerische Mathematik, S. 297
 */
 /** @addtogroup math
 * @{
 */
 /** 
 * @brief Conjugate gradients method for solving linear equation systems.
 * 
 * Finde einen Vektor x fuer A*x=b.
 * Geht momentan nur fuer goSparseMatrix.
 * @todo Spezialimplementierung fuer goSparseMatrix
 * (gibts schon) und Standardimplementierung fuer
 * andere Matrizen.
 *
 * @param A  Matrix A
 * @param b  Vector b
 * @param x  Solution x
 * @param epsilon  Error bound.
 * 
 * @return Error.
 */
template <class MatrixType, class VectorType>
goDouble goMath::goConjugateGradients (const MatrixType& A, const VectorType& b, VectorType& x, goDouble epsilon)
{
    VectorType g;
    VectorType r;
    VectorType p;
    g = b - A*x;
    r = g;
    goDouble abs_r = sqrt(r * r);
    goDouble rho = 0.0;
    goDouble tau = 0.0;
    goDouble sigma = 0.0;
    goDouble t = 0.0;
    goDouble gamma = 0.0;
    goSize_t maxSteps = b.getSize();
    goSize_t step = 0;
    goSparseMatrix AA;
    AA = A;
    while (abs_r > epsilon && step < maxSteps)
    {
        ++step;
        AA.matrixVectorMult (p,r);
        // p = A*r;
//        printf ("A:\n");
//        for (goIndex_t i = 0; i < A.getElementCount(); ++i)
//        {
//            printf ("\t(%d,%d) == %f\n", A.row(i), A.column(i), A.value(i));
//        }
//        printf ("\nr:\n");
//        for (goIndex_t i = 0; i < r.getSize(); ++i)
//        {
//            printf ("\t%f\n", r[i]);
//        }
//        printf ("\np:\n");
//        for (goIndex_t i = 0; i < p.getSize(); ++i)
//        {
//            printf ("\t%f\n", p[i]);
//        }
        rho = p*p;
        sigma = r*p;
        if (sigma == 0.0)
            break;
        tau = g*r;
        t = tau / sigma;
        x += r*t;
        g -= p*t;
        if (tau == 0.0)
            break;
        gamma = (t*t*rho - tau) / tau;
        r = g + r * gamma;
        abs_r = sqrt (r*r);
    }
    if (abs_r > epsilon)
    {
        printf ("goConjugateGradients() did not converge.\n");
        printf ("Writing matrix AA.\n");
        FILE* f;
        f = fopen ("AA.m","w");
        if (!f)
            exit(1);
        fprintf (f, "function A=AA()\n");
        fprintf (f, "A=sparse(%d,%d);", AA.getRowCount(), AA.getColumnCount());
        for (goIndex_t i = 0; i < AA.getElementCount(); ++i)
        {
            fprintf (f, "A(%d,%d) = %f;\n", AA.row(i)+1, AA.column(i)+1, AA.value(i));
        }
        fprintf (f, "end\n");
        fclose(f);
    }
    return abs_r;
}
/** @} */
template goDouble goMath::goConjugateGradients<goSparseMatrix,goVectord>(const goSparseMatrix& A, const goVectord& b, goVectord& x, goDouble);
// template goDouble goMath::goConjugateGradients<goSparseMatrix,goSparseMatrix>(const goSparseMatrix& A, const goSparseMatrix& b, goSparseMatrix& x);
