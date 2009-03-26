#ifndef GOLAPACK_H
#define GOLAPACK_H

#ifndef GOMATH_H
# include <gomath.h>
#endif

#include <f2c.h>
extern "C" 
{
 #include <cblas.h>
 #include <clapack.h>
}

//= gets defined in f2c.h and messes up std::max and goMath::max
#undef max
#undef min

//=
//= These are from the clapack.h header file from netlib's clapack (not ATLAS),
//= which you can find at http://www.netlib.org/clapack/clapack.h
extern "C" {
/* Subroutine */ int sgesvd_(char *jobu, char *jobvt, integer *m, integer *n, 
	real *a, integer *lda, real *s, real *u, integer *ldu, real *vt, 
	integer *ldvt, real *work, integer *lwork, integer *info);
/* Subroutine */ int dgesvd_(char *jobu, char *jobvt, integer *m, integer *n, 
	doublereal *a, integer *lda, doublereal *s, doublereal *u, integer *
	ldu, doublereal *vt, integer *ldvt, doublereal *work, integer *lwork, 
	integer *info);
/* Subroutine */ int sgels_(char *trans, integer *m, integer *n, integer *
	nrhs, real *a, integer *lda, real *b, integer *ldb, real *work, 
	integer *lwork, integer *info);
/* Subroutine */ int dgels_(char *trans, integer *m, integer *n, integer *
	nrhs, doublereal *a, integer *lda, doublereal *b, integer *ldb, 
	doublereal *work, integer *lwork, integer *info);
 
/* Subroutine */ int sgelss_(integer *m, integer *n, integer *nrhs, real *a, 
	integer *lda, real *b, integer *ldb, real *s, real *rcond, integer *
	rank, real *work, integer *lwork, integer *info);

/* Subroutine */ int dgelss_(integer *m, integer *n, integer *nrhs, 
	doublereal *a, integer *lda, doublereal *b, integer *ldb, doublereal *
	s, doublereal *rcond, integer *rank, doublereal *work, integer *lwork,
	 integer *info);

/* Subroutine */ int sposv_(char *uplo, integer *n, integer *nrhs, real *a, 
	integer *lda, real *b, integer *ldb, integer *info);
/* Subroutine */ int dposv_(char *uplo, integer *n, integer *nrhs, doublereal *a, 
	integer *lda, doublereal *b, integer *ldb, integer *info);
}


namespace goMath {

    /*! 
     * \addtogroup mathla
     * @{
     */
    namespace Lapack {

    void logError (integer info, const char* where = 0);

    template <class matrix_type, class pivot_vector>
        bool getrf (matrix_type& A, pivot_vector& ipiv);

    template <class matrix_type, class pivot_vector>
        bool getrs (const matrix_type& A, bool transA, matrix_type& B, const pivot_vector& ipiv);
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
            static bool getrf (const enum CBLAS_ORDER order, const int M, const int N, T* A, const int lda, int *ipiv);
            static bool getrs (const enum CBLAS_ORDER order, const enum CBLAS_TRANSPOSE trans, const int N, const int NRHS, const T* A, const int lda, const int* ipiv, T* B, const int ldb);
            static bool getri (const enum CBLAS_ORDER order, const int N, T* A, const int lda, const int* ipiv);
            static bool gels (char *trans, integer *m, integer *n, integer *
                    nrhs, T *a, integer *lda, T *b, integer *ldb, T *work, 
                    integer *lwork, integer *info);
            static bool gelss (integer *m, integer *n, integer *nrhs, T *a, 
                    integer *lda, T *b, integer *ldb, T *s, T *rcond, integer *
                    rank, T *work, integer *lwork, integer *info);
            static bool posv (char *uplo, integer *n, integer *nrhs, T *a,
                    integer *lda, T *b, integer *ldb, integer *info);
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
    return TypeDriver<typename matrix_type::value_type>::getrf (CblasRowMajor, M, N, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr());
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
    return TypeDriver<typename matrix_type::value_type>::getrs (CblasRowMajor, transA ? CblasTrans : CblasNoTrans, N, NRHS, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr(), B.getPtr(), B.getLeadingDimension());
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
    return TypeDriver<typename matrix_type::value_type>::getri (CblasRowMajor, N, A.getPtr(), A.getLeadingDimension(), ipiv.getPtr());
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
    integer M = A.getColumns();
    integer N = A.getRows();
    integer NRHS = 1;
    char trans = transA ? 'N' : 'T';  //= The other way around since we use row major
    //= This is from the documentation at http://www.netlib.org/lapack/single/sgels.f
    integer LWORK = goMath::max<integer> (1, M * N + goMath::max<integer> (M * N, NRHS));
    goMath::Vector<typename matrix_type::value_type> WORK (LWORK);
    integer info = 0;
    integer lda = A.getLeadingDimension();
    integer ldb = b.getSize();
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
    integer M = A.getColumns();
    integer N = A.getRows();

    integer NRHS = 1;
    //= This is from the documentation at http://www.netlib.org/lapack/single/sgelss.f
    // LWORK >= 3*min(M,N) + max( 2*min(M,N), max(M,N), NRHS ) --- larger for better performance.
    integer LWORK = 3 * goMath::min<integer> (M,N) + goMath::max<integer> (2 * goMath::min<integer> (M,N), goMath::max<integer> (goMath::max<integer> (M,N), NRHS));
    goMath::Vector<typename matrix_type::value_type> WORK (LWORK);
    integer info = 0;
    integer lda = A.getLeadingDimension();
    integer ldb = b.getSize();
    typename matrix_type::value_type rcond = -1.0f;
    integer rank = 0;
    vector_type temp_sv(0);
    if (singularValues)
        singularValues->resize (goMath::min<integer>(M,N));
    else
        temp_sv.resize (goMath::min<integer>(M,N));

    return TypeDriver<typename matrix_type::value_type>::gelss (&M, &N, &NRHS, A.getPtr(), 
            &lda, b.getPtr(), &ldb, singularValues ? singularValues->getPtr() : temp_sv.getPtr(), 
            &rcond, &rank, WORK.getPtr(), &LWORK, &info);
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
    integer n = b.getSize ();
    integer nrhs = 1;
    integer ldb = n;
    integer lda = A.getLeadingDimension ();
    integer info = 0;

    return TypeDriver<typename matrix_type::value_type>::posv (&uplo, &n, &nrhs, A.getPtr (),
                    &lda, b.getPtr (), &ldb, &info);
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
    integer n = b.getColumns ();
    integer nrhs = b.getRows ();
    integer ldb = b.getLeadingDimension ();
    integer lda = A.getLeadingDimension ();
    integer info = 0;

    return TypeDriver<typename matrix_type::value_type>::posv (&uplo, &n, &nrhs, A.getPtr (),
                    &lda, b.getPtr (), &ldb, &info);
}

/** @} */
#endif
