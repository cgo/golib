#ifndef GOLAPACK_H
#define GOLAPACK_H

#include <f2c.h>
extern "C" 
{
 #include <cblas.h>
 #include <clapack.h>
}

extern "C" {
/* Subroutine */ int sgesvd_(char *jobu, char *jobvt, integer *m, integer *n, 
	real *a, integer *lda, real *s, real *u, integer *ldu, real *vt, 
	integer *ldvt, real *work, integer *lwork, integer *info);
/* Subroutine */ int dgesvd_(char *jobu, char *jobvt, integer *m, integer *n, 
	doublereal *a, integer *lda, doublereal *s, doublereal *u, integer *
	ldu, doublereal *vt, integer *ldvt, doublereal *work, integer *lwork, 
	integer *info);
}

namespace goMath { namespace Lapack {

    template <class matrix_type, class pivot_vector>
        bool getrf (matrix_type& A, pivot_vector& ipiv);

    template <class matrix_type, class pivot_vector>
        bool getrs (const matrix_type& A, bool transA, matrix_type& B, const pivot_vector& ipiv);

    template <class T>
    class TypeDriver
    {
        public:
            static bool getrf (const enum CBLAS_ORDER order, const int M, const int N, T* A, const int lda, int *ipiv);
            static bool getrs (const enum CBLAS_ORDER order, const enum CBLAS_TRANSPOSE trans, const int N, const int NRHS, const T* A, const int lda, const int* ipiv, T* B, const int ldb);
    };

}; };

template <class matrix_type, class pivot_vector>
bool goMath::Lapack::getrf (matrix_type& A, pivot_vector& ipiv)
{
    int M = A.getRows();
    int N = A.getColumns();
    if (ipiv.getSize() != N)
        ipiv.resize (N);
    return TypeDriver<typename matrix_type::value_type>::getrf (CblasRowMajor, M, N, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr());
}

/** \addtogroup math
 * @brief Lapack getrs.
 *
 * Calculates \f$$ B^top \gets A x = B^top \f$$, assuming A is LU-decomposed e.g. with
 * getrf(). Implemented for goFloat and goDouble. pivot_vector must be of type \c int,
 * and provide getSize(), resize() as well as getPtr() methods (such as goVector).
 * matrix_type must essentially be goMatrix.
 * 
 * @note B contains the right hand side vectors in its \b rows. Always remember this.
 * This is apparently an effect of using row major order which stems from ATLAS.
 *
 * @param A         Matrix A
 * @param transA    Use A transposed
 * @param B         Right hand side (the right hand side vectors are in the \c rows of B, not the columns!)
 * @param ipiv      Pivot vector (from getrf()).
 * 
 * @return True if successful, false otherwise.
 */
template <class matrix_type, class pivot_vector>
bool goMath::Lapack::getrs (const matrix_type& A, bool transA, matrix_type& B, const pivot_vector& ipiv)
{
    int N = transA ? A.getRows() : A.getColumns();
    //= For ATLAS, B is always just a concatenation of columns -- that is why
    //= the solution vectors must be in the ROWS of B, since we are working
    //= with row-major order, as usual in C. 
    int NRHS = B.getRows();
    // int M = transA ? A.getColumns() : A.getRows();
    // int N = transA ? A.getRows() : A.getColumns();
//    return TypeDriver<typename matrix_type::value_type>::getrs (CblasRowMajor, transA ? CblasTrans : CblasNoTrans, N, NRHS, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr(), B.getPtr(), B.getLeadingDimension());
    return TypeDriver<typename matrix_type::value_type>::getrs (CblasRowMajor, transA ? CblasTrans : CblasNoTrans, N, NRHS, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr(), B.getPtr(), B.getLeadingDimension());
}
#endif
