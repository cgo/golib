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

    template<> bool TypeDriver<goFloat>::getri (const enum CBLAS_ORDER order, const int N, goFloat* A, const int lda, const int* ipiv)
    {
        if (clapack_sgetri (CblasRowMajor, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::getri (const enum CBLAS_ORDER order, const int N, goDouble* A, const int lda, const int* ipiv)
    {
        if (clapack_dgetri (CblasRowMajor, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::gels (char *trans, integer *m, integer *n, integer *
	nrhs, goDouble *a, integer *lda, goDouble *b, integer *ldb, 
	goDouble *work, integer *lwork, integer *info)
    {
        dgels_ (trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info);
        if (*info != 0)
        {
            if (*info > 0)
            {
                goLog::warning ("goMath::Lapack::TypeDriver::gels(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goFloat>::gels (char *trans, integer *m, integer *n, integer *
            nrhs, goFloat *a, integer *lda, goFloat *b, integer*ldb, goFloat *work, 
            integer*lwork, integer*info)
    {
        sgels_ (trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info);
        if (*info != 0)
        {
            if (*info > 0)
            {
                goLog::warning ("goMath::Lapack::TypeDriver::gels(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goFloat>::gelss (integer *m, integer *n, integer *nrhs, goFloat *a, 
                    integer *lda, goFloat *b, integer *ldb, goFloat *s, goFloat *rcond, integer *
                    rank, goFloat *work, integer *lwork, integer *info)
    {
        sgelss_ (m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, lwork, info);
        if (*info != 0)
        {
            if (*info > 0)
            {
                goLog::warning ("goMath::Lapack::TypeDriver::gelss(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::gelss (integer *m, integer *n, integer *nrhs, goDouble *a, 
                    integer *lda, goDouble *b, integer *ldb, goDouble *s, goDouble *rcond, integer *
                    rank, goDouble *work, integer *lwork, integer *info)
    {
        dgelss_ (m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, lwork, info);
        if (*info != 0)
        {
            if (*info > 0)
            {
                goLog::warning ("goMath::Lapack::TypeDriver::gelss(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }


    template<> bool TypeDriver<goFloat>::posv (char *uplo, integer *n, integer *nrhs, goFloat *a,
                    integer *lda, goFloat *b, integer *ldb, integer *info)
    {
        printf ("posv with %c %d %d %d %d\n", *uplo, *n, *nrhs, *lda, *ldb);
        sposv_ (uplo, n, nrhs, a, lda, b, ldb, info);
        if (*info != 0)
        {
            logError (*info, "goMath::Lapack::TypeDriver::sposv()");
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::posv (char *uplo, integer *n, integer *nrhs, goDouble *a,
                    integer *lda, goDouble *b, integer *ldb, integer *info)
    {
        dposv_ (uplo, n, nrhs, a, lda, b, ldb, info);
        if (*info != 0)
        {
            logError (*info, "goMath::Lapack::TypeDriver::dposv()");
            return false;
        }
        return true;
    }

    void logError (integer info, const char* where)
    {
        if (info < 0)
        {
            goString s;
            if (where)
                s = where;

            s += ": Illegal value of one or more arguments. No computation performed.";
            goLog::warning (s.toCharPtr ());
        }
        else if (info > 0)
        {
            goString s;
            if (where)
                s = where;

            s += ": Failure in the course of computation.";
            goLog::warning (s.toCharPtr ());
        }
    }

}; };

