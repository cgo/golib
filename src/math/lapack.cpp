// #include <gomath.h>
#include <gotypes.h>
#include <golapack.h>

namespace goMath { namespace Lapack {

    template<> bool TypeDriver<goFloat>::getrf (const enum CBLAS_ORDER order, const int M, const int N, goFloat* A, const int lda, int *ipiv)
    {
        if (clapack_sgetrf (order, M, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::getrf (const enum CBLAS_ORDER order, const int M, const int N, goDouble* A, const int lda, int *ipiv)
    {
        if (clapack_dgetrf (order, M, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goFloat>::getrs (const enum CBLAS_ORDER order, const enum CBLAS_TRANSPOSE trans, const int N, const int NRHS, const goFloat* A, const int lda, const int* ipiv, goFloat* B, const int ldb)
    {
        if (clapack_sgetrs (order, trans, N, NRHS, A, lda, ipiv, B, ldb) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::getrs (const enum CBLAS_ORDER order, const enum CBLAS_TRANSPOSE trans, const int N, const int NRHS, const goDouble* A, const int lda, const int* ipiv, goDouble* B, const int ldb)
    {
        if (clapack_dgetrs (order, trans, N, NRHS, A, lda, ipiv, B, ldb) != 0)
        {
            return false;
        }
        return true;
    }
}; };

