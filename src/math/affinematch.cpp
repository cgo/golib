/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
bool goMath::affineMatch (const goMath::Matrix<T>& X1, const goMath::Matrix<T>& X2, goMath::Matrix<T>& A, Vector<T>& t)
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

    goMath::Matrix<T> X2T_one (n, 1);
    goMath::Matrix<T> X1T_one (n, 1);
    goMath::Matrix<T> X2TX1;
    goMath::Matrix<T> X1TX1;
    //= X2'*X1
    goMath::matrixMult<T> (T(1), X2, true, X1, false, 0.0f, X2TX1);
    //= X1'*X1
    goMath::matrixMult<T> (T(1), X1, true, X1, false, 0.0f, X1TX1);

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
    goMath::matrixMult<T> (-m_, X2T_one, false, X1T_one, true, 1.0f, X2TX1);
    //= X1'*X1 - 1/m * X1T_one * X1T_one'
    goMath::matrixMult<T> (-m_, X1T_one, false, X1T_one, true, 1.0f, X1TX1);
    
    X1TX1.invert ();
   
    //= A = (X2'*X1 - 1/m * X2T_one * X1T_one') * inv(X1'*X1 - 1/m * X1T_one * X1T_one')
    goMath::matrixMult<T> (T(1), X2TX1, false, X1TX1, false, T(0), A);
    //= t = 1/m * (A * X1T_one - X2T_one)
    goMath::matrixMult<T> (m_, A, false, X1T_one, false, -m_, X2T_one);

    assert (X2T_one.getRows() == n);
    assert (X2T_one.getColumns() == 1);

    X2T_one.copyColumn (0, t);

    return true;
}

/** 
 * @brief "Conditional" version of affine match of q to s.
 *
 * Calculate the affine transformation with A and t that minimises
 * \f$ \| s - (q\cdot A^\top - \mathbf{1}\cdot t^\top) \|  + \beta \cdot \| s_2 - (q_2\cdot A^\top - \mathbf{1}\cdot t^\top) \|\f$,
 * with \f$ \mathbf{1} \f$ a vector of ones of appropriate size.
 *
 * Instantiated for goFloat and goDouble.
 *
 * @param q Point configuration matrix q
 * @param s Point configuration matrix s
 * @param beta Scalar factor
 * @param q2 Point configuration matrix q2
 * @param s2 Point configuration matrix s2
 * @param A  On return, contains the matrix A 
 * @param t  On return, contains the translation vector t
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::affineMatch(
        const goMath::Matrix<T>& q,
        const goMath::Matrix<T>& s,
        goDouble beta,
        const goMath::Matrix<T>& q2,
        const goMath::Matrix<T>& s2,
        goMath::Matrix<T>& A,
        Vector<T>& t)
{
    if (s.getRows() != q.getRows() || 
        s.getColumns() != q.getColumns())
    {
        goLog::warning ("goMath::affineMatch(): s and q do not have matching size.");
        return false;
    }
    if (s2.getRows() != q2.getRows() || 
        s2.getColumns() != q2.getColumns())
    {
        goLog::warning ("goMath::affineMatch(): s2 and q2 do not have matching size.");
        return false;
    }


    //= Matlab test version:
#if 0
    matlab.putMatrix (s, "s");
    matlab.putMatrix (q, "q");
    matlab.putMatrix (s2, "s2");
    matlab.putMatrix (q2, "q2");
    matlab.putDouble (beta, "beta");
    matlab.putDouble ((goDouble)s.getRows(), "m");
    matlab.putDouble ((goDouble)s2.getRows(), "m2");
    goString str;
    str.resize (1024);
    matlab.matlabCall (" \
            o1 = ones(int32(m),1); \
            o2 = ones(int32(m2),1); \
            A = (s'*q + beta*s2'*q2 - 1 / (m + beta*m2) * (s'*o1 + beta*s2'*o2) * (o1'*q + beta*o2'*q2)) * \
                inv(q'*q + beta*q2'*q2 - 1 / (m + beta*m2) * (q'*o1 + beta*q2'*o2) * (o1'*q + beta*o2'*q2)); \
            t = (A*q'*o1 - s'*o1)/m + beta*(A*q2'*o2 - s2'*o2)/(m * beta*m2);", &str);
    if (strlen(str.toCharPtr()) > 0)
    {
        printf ("%s\n", str.toCharPtr());
    }
    matlab.getMatrix (A, "A");
    matlab.getVector (&t, "t");
    return;
#endif

    goMath::Matrix<T> b1, b2, b; //= b1 = ones^T * q 
                           //= b2 = ones^T * q2
                           //= b = b1 + beta * b2
    {
        q2.sum (0, b2);
        q.sum (0, b1);
        b = b1 + b2 * beta;
    }

    //= factor = 1 / (m + beta*m2)
    T factor = T(1) / (T(s.getRows()) + beta * T(s2.getRows()));

    goMath::Matrix<T> a1, a2, a; //= a1 = (s^T*ones)^T
                           //= a2 = (s2^T*ones)^T
                           //= a = a1 + beta * a2
    {
        s2.sum (0, a2);
        s.sum (0, a1);
        a = a1 + a2 * beta;
    }
   
    goMath::Matrix<T> B;
    goMath::matrixMult<T> (T(factor), a, true, b, false, T(0), B);
    goMath::matrixMult<T> (T(beta), s2, true, q2, false, T(-1), B);
    goMath::matrixMult<T> (T(1), s, true, q, false, T(1), B);
    
    goMath::Matrix<T> C;
    goMath::matrixMult<T> (factor, b, true, b, false, T(0), C);
    goMath::matrixMult<T> (T(beta), q2, true, q2, false, T(-1), C);
    goMath::matrixMult<T> (T(1), q, true, q, false, T(1), C);

    C.invert ();
    
    goMath::matrixMult<T> (T(1), B, false, C, false, T(0), A);

    //= t:
    goMath::matrixMult<T> (factor*beta, A, false, b2, true, T(0), B);
    goMath::matrixMult<T> (factor, A, false, b1, true, T(1), B);
    a.getTranspose (C);
    C *= -factor;

    B += C;
    B.copyColumn (0, t);

    return true;
}

template bool goMath::affineMatch<goFloat> (const goMath::Matrix<goFloat>&, const goMath::Matrix<goFloat>&, 
                                            goMath::Matrix<goFloat>&, Vector<goFloat>&);
template bool goMath::affineMatch<goDouble> (const goMath::Matrix<goDouble>&, const goMath::Matrix<goDouble>&, 
                                            goMath::Matrix<goDouble>&, Vector<goDouble>&);
template bool goMath::affineMatch<goFloat>(
        const goMath::Matrix<goFloat>&,
        const goMath::Matrix<goFloat>&,
        goDouble ,
        const goMath::Matrix<goFloat>&,
        const goMath::Matrix<goFloat>&,
        goMath::Matrix<goFloat>&,
        Vector<goFloat>&);
template bool goMath::affineMatch<goDouble>(
        const goMath::Matrix<goDouble>&,
        const goMath::Matrix<goDouble>&,
        goDouble ,
        const goMath::Matrix<goDouble>&,
        const goMath::Matrix<goDouble>&,
        goMath::Matrix<goDouble>&,
        Vector<goDouble>&);
