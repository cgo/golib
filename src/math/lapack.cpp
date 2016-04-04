/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


// #include <gomath.h>
#include <goconfig.h>
#include <gotypes.h>
#include <golapack.h>

#include <golib_clapack.h>

namespace goMath { namespace Lapack {

    template<> bool TypeDriver<goFloat>::getrf (int order, lapack_int M, lapack_int N, goFloat* A, lapack_int lda, lapack_int *ipiv)
    {
        if (LAPACKE_sgetrf (order, M, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::getrf (int order, lapack_int M, lapack_int N, goDouble* A, lapack_int lda, lapack_int *ipiv)
    {
        if (LAPACKE_dgetrf (order, M, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goFloat>::getrs (int order, char trans, lapack_int N, lapack_int NRHS, const goFloat* A, lapack_int lda, const lapack_int* ipiv, goFloat* B, lapack_int ldb)
    {
        if (LAPACKE_sgetrs (order, trans, N, NRHS, A, lda, ipiv, B, ldb) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::getrs (int order, char trans, lapack_int N, lapack_int NRHS, const goDouble* A, lapack_int lda, const lapack_int* ipiv, goDouble* B, lapack_int ldb)
    {
        if (LAPACKE_dgetrs (order, trans, N, NRHS, A, lda, ipiv, B, ldb) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goFloat>::getri (int order, lapack_int N, goFloat* A, lapack_int lda, const lapack_int* ipiv)
    {
        if (LAPACKE_sgetri (order, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::getri (int order, lapack_int N, goDouble* A, lapack_int lda, const lapack_int* ipiv)
    {
        if (LAPACKE_dgetri (order, N, A, lda, ipiv) != 0)
        {
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::gels (int matrix_order, char trans, lapack_int m,
            lapack_int n, lapack_int nrhs, goDouble* a,
            lapack_int lda, goDouble* b, lapack_int ldb)
    {
    	lapack_int const info = LAPACKE_dgels (matrix_order, trans, m, n, nrhs, a, lda, b, ldb);
        if (0 != info)
        {
            if (info > 0)
            {
            	goLog::warning ("goMath::Lapack::TypeDriver::gels(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goFloat>::gels (int matrix_order, char trans, lapack_int m,
            lapack_int n, lapack_int nrhs, float* a,
            lapack_int lda, float* b, lapack_int ldb)
    {
        lapack_int const info = LAPACKE_sgels (matrix_order, trans, m, n, nrhs, a, lda, b, ldb);
        if (info != 0)
        {
            if (info > 0)
            {
                goLog::warning ("goMath::Lapack::TypeDriver::gels(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goFloat>::gelss (int matrix_order, lapack_int m, lapack_int n,
            lapack_int nrhs, goFloat* a, lapack_int lda, goFloat* b,
            lapack_int ldb, goFloat* s, goFloat rcond,
            lapack_int* rank)
    {
        lapack_int const info = LAPACKE_sgelss (matrix_order, m, n, nrhs, a, lda, b, ldb, s, rcond, rank);
        if (info != 0)
        {
            if (info > 0)
            {
                goLog::warning ("goMath::Lapack::TypeDriver::gelss(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::gelss (int matrix_order, lapack_int m, lapack_int n,
            lapack_int nrhs, goDouble* a, lapack_int lda, goDouble* b,
            lapack_int ldb, goDouble* s, goDouble rcond,
            lapack_int* rank)
    {
        lapack_int const info = LAPACKE_dgelss (matrix_order, m, n, nrhs, a, lda, b, ldb, s, rcond, rank);
        if (info != 0)
        {
            if (info > 0)
            {
                goLog::warning ("goMath::Lapack::TypeDriver::gelss(): Matrix does not have full rank.");
            }
            return false;
        }
        return true;
    }


    template<> bool TypeDriver<goFloat>::posv (int matrix_order, char uplo, lapack_int n,
            lapack_int nrhs, goFloat* a, lapack_int lda, goFloat* b,
            lapack_int ldb)
    {
        // printf ("posv with %c %d %d %d %d\n", *uplo, *n, *nrhs, *lda, *ldb);
        lapack_int const info = LAPACKE_sposv (matrix_order, uplo, n, nrhs, a, lda, b, ldb);
        if (info != 0)
        {
            logError (info, "goMath::Lapack::TypeDriver::sposv()");
            return false;
        }
        return true;
    }

    template<> bool TypeDriver<goDouble>::posv (int matrix_order, char uplo, lapack_int n,
            lapack_int nrhs, goDouble* a, lapack_int lda, goDouble* b,
            lapack_int ldb)
    {
        lapack_int const info = LAPACKE_dposv (matrix_order, uplo, n, nrhs, a, lda, b, ldb);
        if (info != 0)
        {
            logError (info, "goMath::Lapack::TypeDriver::dposv()");
            return false;
        }
        return true;
    }

    void logError (lapack_int info, const char* where)
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

