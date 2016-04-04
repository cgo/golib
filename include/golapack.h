/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOLAPACK_H
#define GOLAPACK_H

#ifndef GOMATH_H
# include <gomath.h>
#endif

#include <goconfig.h>
#ifdef HAVE_F2C_H
# include <f2c.h>
#endif

//= gets defined in f2c.h and messes up std::max and goMath::max
#undef max
#undef min

#include <golib_clapack.h>

#ifdef OSX
// #include <clapack.h>
#endif

namespace goMath {

    /*! 
     * \addtogroup mathla
     * @{
     */
    namespace Lapack {

    void logError (lapack_int info, const char* where = 0);

    template <class matrix_type, class pivot_vector>
        bool getrf (matrix_type& A, pivot_vector& ipiv);

    template <class matrix_type, class pivot_vector>
        bool getrs (const matrix_type& A, bool transA, matrix_type& B, const pivot_vector& ipiv);
    template <class matrix_type, class pivot_vector>
        bool getrs (const matrix_type& A, bool transA, goMath::Vector<typename matrix_type::value_type>& B, const pivot_vector& ipiv);
    template <class matrix_type, class pivot_vector>
        bool getri (matrix_type& A, const pivot_vector& ipiv);

    template <class matrix_type, class vector_type>
        bool gels (matrix_type& A, bool transA, vector_type& b);

    // FIXME
    template <class matrix_type, class vector_type>
        bool gelss (matrix_type& A, bool transA, vector_type& b, vector_type* singularValues = 0);
    
    template <class matrix_type, class vector_type>
        bool posv (matrix_type& A, vector_type& b);

    template <class matrix_type>
        bool posv (matrix_type& A, matrix_type& b);


    template <class T>
    class TypeDriver
    {
        public:
            static bool getrf (int order, lapack_int M, lapack_int N, T* A, lapack_int lda, lapack_int *ipiv);
            static bool getrs (int order, char trans, lapack_int N, lapack_int NRHS, const T* A, lapack_int lda, const lapack_int* ipiv, T* B, lapack_int ldb);
            static bool getri (int order, lapack_int N, T* A, lapack_int lda, const lapack_int* ipiv);
            static bool gels (int matrix_order, char trans, lapack_int m,
                    lapack_int n, lapack_int nrhs, T* a,
                    lapack_int lda, T* b, lapack_int ldb);
            static bool gelss (int matrix_order, lapack_int m, lapack_int n,
                    lapack_int nrhs, T* a, lapack_int lda, T* b,
                    lapack_int ldb, T* s, T rcond,
                    lapack_int* rank);
            static bool posv (int matrix_order, char uplo, lapack_int n,
                    lapack_int nrhs, T* a, lapack_int lda, T* b,
                    lapack_int ldb);
    };

}; 
/*! @} */
};

/**
 * \addtogroup mathla
 * @{
 */
/** 
 * @brief Lapack getrf.
 * 
 * @note Uses ATLAS' clapack implementation.
 *
 * Replaces A by its LU-decomposed form,
 * \f$ A \gets LU, \, ipiv \gets P \f$
 * so that
 * \f$ AP = LU \, . \f$
 * U is unit diagonal, P pivots columns.
 * @param A Matrix to be decomposed.
 * @param ipiv Pivot vector (filled by this function).
 * 
 * @return True if successful, false otherwise.
 */
template <class matrix_type, class pivot_vector>
bool goMath::Lapack::getrf (matrix_type& A, pivot_vector& ipiv)
{
    int M = A.getRows();
    int N = A.getColumns();
    if (ipiv.getSize() != N)
        ipiv.resize (N);
    return TypeDriver<typename matrix_type::value_type>::getrf (matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR, M, N, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr());
}

/** 
 * @brief Lapack getrs.
 *
 * @note Uses ATLAS' clapack implementation.
 *
 * Solves \f$ A x = B^\top \f$ for x, assuming A is LU-decomposed e.g. with
 * getrf(). Implemented for goFloat and goDouble. pivot_vector must be of type \c int,
 * and provide getSize(), resize() as well as getPtr() methods (such as goMath::Vector).
 * matrix_type must essentially be goMath::Matrix.
 * 
 * @note B contains the right hand side vectors in its \b rows. Always remember this.
 * This is apparently an effect of using row major order which stems from ATLAS.
 *
 * @param A         Matrix A
 * @param transA    Use A transposed
 * @param B         Right hand side (the right hand side vectors are in the \c rows of B, not the columns!). On success, the rows contain the solutions.
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
    return TypeDriver<typename matrix_type::value_type>::getrs (matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR, transA ? 'T' : 'N', N, NRHS, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr(), B.getPtr(), B.getLeadingDimension());
}

template <class matrix_type, class pivot_vector>
bool goMath::Lapack::getrs (const matrix_type& A, bool transA, goMath::Vector<typename matrix_type::value_type>& b, const pivot_vector& ipiv)
{
    int N = transA ? A.getRows() : A.getColumns();
    //= For ATLAS, B is always just a concatenation of columns -- that is why
    //= the solution vectors must be in the ROWS of B, since we are working
    //= with row-major order, as usual in C. 
    int NRHS = 1; // B.getRows();
    // int M = transA ? A.getColumns() : A.getRows();
    // int N = transA ? A.getRows() : A.getColumns();
    return TypeDriver<typename matrix_type::value_type>::getrs (matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR, transA ? 'T' : 'N', N, NRHS, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr(), b.getPtr(), b.getSize ());
}

/** 
 * @brief Lapack getri, invert a LU-decomposed matrix.
 *
 * @note Uses ATLAS' clapack implementation.
 *
 * The matrix must be in LU-form created by getrf().
 * 
 * @param A Matrix to invert. Must be in LU form and must be quadratic.
 * @param ipiv Pivot vector as created by getrf().
 * 
 * @return True if successful, false otherwise.
 */
template <class matrix_type, class pivot_vector>
bool goMath::Lapack::getri (matrix_type& A, const pivot_vector& ipiv)
{
    if (A.getRows() != A.getColumns())
    {
        return false;
    }
    int N = A.getRows();
    return TypeDriver<typename matrix_type::value_type>::getri (matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR, N, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr());
}

/** 
 * @brief Lapack gels. Least square solution of a linear system.
 *
 * For \f$ A \in R^{m \times n}\f$ solves
 * \f$ \min_x \|Ax - b\|\f$
 * or
 * \f$ \min_x \|A^T x - b\|\f$
 * if m >= n, or it finds the minimum norm solution of
 * \f$ Ax = b \f$ or 
 * \f$ A^T x = b \f$
 * if m < n, so that \f$ Ax = b\f$ is underdetermined.
 * A must have full rank.
 * 
 * @param A Matrix A. Will be overwritten by QR or LQ decompositions.
 * @param transA If true, A is used transposed.
 * @param b Right hand side vector. Will be overwritten with the solution vector.
 * 
 * @return True if successful, false otherwise.
 * @bug Appears not to work -- needs testing (examples/lapack.cpp)
 */
template <class matrix_type, class vector_type>
bool goMath::Lapack::gels (matrix_type& A, bool transA, vector_type& b)
{
    //= Note: gels_() expects column major -- so MxN means in C columns x rows and 
    //= transposed usage of A.
    lapack_int M = A.getColumns();
    lapack_int N = A.getRows();
    lapack_int NRHS = 1;
    char trans = transA ? 'N' : 'T';  //= The other way around since we use row major
    //= This is from the documentation at http://www.netlib.org/lapack/single/sgels.f
    lapack_int LWORK = goMath::max<lapack_int> (1, M * N + goMath::max<lapack_int> (M * N, NRHS));
    goMath::Vector<typename matrix_type::value_type> WORK (LWORK);
    lapack_int info = 0;
    lapack_int lda = A.getLeadingDimension();
    lapack_int ldb = b.getSize();
    return TypeDriver<typename matrix_type::value_type>::gels (&trans, &M, &N, &NRHS, A.getPtr(), &lda, b.getPtr(), &ldb, WORK.getPtr(), &LWORK, &info);
}

/** 
 * @brief Lapack gelss.
 *
 * @note NEEDS TESTING, UNTESTED. Possibly the LDA is wrong if the matrix is not
 * square (which is true in general).
 * 
 * @param A 
 * @param transA 
 * @param b 
 * @param singularValues 
 * 
 * @return 
 */
template <class matrix_type, class vector_type>
bool goMath::Lapack::gelss (matrix_type& A, bool transA, vector_type& b, vector_type* singularValues)
{
    if (!transA)
        A.transpose();

    //= Note: gelss_() expects column major -- so MxN means in C columns x rows and 
    //= transposed usage of A.
    lapack_int M = A.getColumns();
    lapack_int N = A.getRows();

    lapack_int NRHS = 1;
    //= This is from the documentation at http://www.netlib.org/lapack/single/sgelss.f
    // LWORK >= 3*min(M,N) + max( 2*min(M,N), max(M,N), NRHS ) --- larger for better performance.
    lapack_int LWORK = 3 * goMath::min<lapack_int> (M,N) + goMath::max<lapack_int> (2 * goMath::min<lapack_int> (M,N), goMath::max<lapack_int> (goMath::max<lapack_int> (M,N), NRHS));
    goMath::Vector<typename matrix_type::value_type> WORK (LWORK);
    lapack_int info = 0;
    lapack_int lda = A.getLeadingDimension();
    lapack_int ldb = b.getSize();
    typename matrix_type::value_type rcond = -1.0f;
    lapack_int rank = 0;
    vector_type temp_sv(0);
    if (singularValues)
        singularValues->resize (goMath::min<lapack_int>(M,N));
    else
        temp_sv.resize (goMath::min<lapack_int>(M,N));

    int const order = matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR;
    return TypeDriver<typename matrix_type::value_type>::gelss(order,M,N,NRHS,A.getPtr(),lda,b.getPtr(),ldb,singularValues ? singularValues->getPtr() : temp_sv.getPtr, rcond, &rank);

//    return TypeDriver<typename matrix_type::value_type>::gelss (&M, &N, &NRHS, A.getPtr(),
//            &lda, b.getPtr(), &ldb, singularValues ? singularValues->getPtr() : temp_sv.getPtr(),
//            &rcond, &rank, WORK.getPtr(), &LWORK, &info);
}

/** 
 * @brief Lapack *posv procedure for solving symmetric linear systems.
 *
 * @param A Symmetric matrix, no special storage, must be upper right.
 * @param b Right hand side vector. Contains the solution if the method returns true.
 * 
 * @return True if successful, false otherwise.
 */
template <class matrix_type, class vector_type>
bool goMath::Lapack::posv (matrix_type& A, vector_type& b)
{

    //= Translated by f2c, so this uses Fortran storage (column-first). Therefore, when
    //= saying UPLO = 'L', i.e. lower triangular, the storage must be in 'U' in the row-major
    //= storage scheme. I.e., A is used transposed, but since it is symmetric it does not matter.
    //= A is assumed to contain the data in the upper triangle (in row major, C-like storage).
    char uplo = 'L';
    lapack_int n = b.getSize ();
    lapack_int nrhs = 1;
    lapack_int ldb = n;
    lapack_int lda = A.getLeadingDimension ();
    lapack_int info = 0;

    int const order = matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR;
    return TypeDriver<typename matrix_type::value_type>::posv(order,uplo,n,nrhs,A.getPtr(),lda,b.getPtr(),ldb);

//    return TypeDriver<typename matrix_type::value_type>::posv (&uplo, &n, &nrhs, A.getPtr (),
//                    &lda, b.getPtr (), &ldb, &info);
}

/** 
 * @brief Lapack *posv procedure for solving symmetric linear systems.
 *
 * @param A Symmetric matrix, no special storage, must be upper right.
 * @param b Contains the right hand side vectors <b>in its rows</b>. This is due
 * to the actual lapack routine using column major storage and we use the more C-like row major.
 * Contains the solutions if the method returns true. The solutions are contained <b>in the rows</b>,
 * again because of the underlying routine using column major storage.
 * 
 * @return True if successful, false otherwise.
 */
template <class matrix_type>
bool goMath::Lapack::posv (matrix_type& A, matrix_type& b)
{

    //= Translated by f2c, so this uses Fortran storage (column-first). Therefore, when
    //= saying UPLO = 'L', i.e. lower triangular, the storage must be in 'U' in the row-major
    //= storage scheme. I.e., A is used transposed, but since it is symmetric it does not matter.
    //= A is assumed to contain the data in the upper triangle (in row major, C-like storage).
    char uplo = 'L';

    //= cols and rows are swapped here, since the type driver uses LAPACK's columns first storage
    lapack_int n = b.getColumns ();
    lapack_int nrhs = b.getRows ();
    lapack_int ldb = b.getLeadingDimension ();
    lapack_int lda = A.getLeadingDimension ();
    lapack_int info = 0;

    int const order = matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR;
    return TypeDriver<typename matrix_type::value_type>::posv(order,uplo,n,nrhs,A.getPtr(),lda,b.getPtr(),ldb);
    //return TypeDriver<typename matrix_type::value_type>::posv (&uplo, &n, &nrhs, A.getPtr (),
    //                &lda, b.getPtr (), &ldb, &info);
}

/** @} */
#endif
