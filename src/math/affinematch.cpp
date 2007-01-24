#include <gomath.h>
#include <gomatrix.h>
#include <govector.h>

/** 
 * @brief Finds A,t so that \f$ \|X2 - (X1\cdot A^\top - \mathbf{1}\cdot t^\top))\|^2 \f$ is minimal.
 * 
 * X1 and X2 are configuration matrices containing the coordinates of m points
 * in n dimensions, one point per row:  \f$ X1, X2 \in R^{m \times n}\f$.
 *
 * @param X1 Configuration matrix 1
 * @param X2 Configuration matrix 2
 * @param A  Transform matrix, return value.
 * @param t  Translation vector, return value.
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::affineMatch (const goMatrix<T>& X1, const goMatrix<T>& X2, goMatrix<T>& A, goVector<T>& t)
{
    if (X1.getRows() != X2.getRows() ||
        X1.getColumns() != X2.getColumns())
    {
        goLog::warning ("goMath::affineMatch(): X1 and X2 must have equal dimensions.");
        return false;
    }

    goSize_t m = X1.getRows();
    goSize_t n = X1.getColumns();
    T m_ = T(1) / T(m);

    goMatrix<T> X2T_one (n, 1);
    goMatrix<T> X1T_one (n, 1);
    goMatrix<T> X2TX1;
    goMatrix<T> X1TX1;
    //= X2'*X1
    goMatrixMult<T> (T(1), X2, true, X1, false, 0.0f, X2TX1);
    //= X1'*X1
    goMatrixMult<T> (T(1), X1, true, X1, false, 0.0f, X1TX1);

    X2T_one.fill (T(0));
    //= Sum all points over all dimensions:
    for (goSize_t i = 0; i < m; ++i)
    {
        for (goSize_t j = 0; j < n; ++j)
        {
            X2T_one(j,0) += X2(i,j);
            X1T_one(j,0) += X1(i,j);
        }
    }

    //= X2'*X1 - 1/m * X2T_one * X1T_one'
    goMatrixMult<T> (-m_, X2T_one, false, X1T_one, true, 1.0f, X2TX1);
    //= X1'*X1 - 1/m * X1T_one * X1T_one'
    goMatrixMult<T> (-m_, X1T_one, false, X1T_one, true, 1.0f, X1TX1);
    
    X1TX1.invert ();
   
    //= A = (X2'*X1 - 1/m * X2T_one * X1T_one') * inv(X1'*X1 - 1/m * X1T_one * X1T_one')
    goMatrixMult<T> (T(1), X2TX1, false, X1TX1, false, T(0), A);
    //= t = 1/m * (A * X1T_one - X2T_one)
    goMatrixMult<T> (m_, A, false, X1T_one, false, -m_, X2T_one);

    assert (X2T_one.getRows() == n);
    assert (X2T_one.getColumns() == 1);

    X2T_one.copyColumn (0, t);

    return true;
}

template bool goMath::affineMatch<goFloat> (const goMatrix<goFloat>&, const goMatrix<goFloat>&, 
                                            goMatrix<goFloat>&, goVector<goFloat>&);
template bool goMath::affineMatch<goDouble> (const goMatrix<goDouble>&, const goMatrix<goDouble>&, 
                                            goMatrix<goDouble>&, goVector<goDouble>&);
