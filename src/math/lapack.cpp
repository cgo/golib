/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


// #include <gomath.h>
#include <goconfig.h>
#include <gotypes.h>
#include <golapack.h>
#include <gomatrix.h>
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

    template<> bool TypeDriver<goFloat>::heev (
      int matrix_order, char jobz, char uplo, lapack_int n,
      lapack_complex_float* a, lapack_int lda, float* w )
    {
        lapack_int const info = LAPACKE_cheev (matrix_order, jobz, uplo,
          n, a, lda, w);
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


    template <>
    bool heev (goMatrix<goComplexf> const& A,
        std::vector<goVector<goComplexf> >* eigenvectors_ret,
        goVector<goFloat>& eigenvalues_ret)
    {
        typedef goMatrix<goComplexf> matrix_type;

        if (A.getColumns() != A.getRows())
        {
            return false;
        }

        lapack_int const n = A.getRows();
        lapack_int const nm = A.getRows();
        char const jobz = eigenvectors_ret ? 'V' : 'N';
        lapack_complex_float* a = new lapack_complex_float [nm * nm];
        //= I suppose we store in column-major (from Fortran code).
        {
            goSize_t i;
            goSize_t j;
            for (i = 0; i < A.getRows(); ++i)
            {
                for (j = 0; j < A.getColumns(); ++j)
                {
                    lapack_complex_float_real(a[j + i * n]) = A(i, j).re();
                    lapack_complex_float_imag(a[j + i * n]) = A(i, j).im();
                }
            }
        }

        eigenvalues_ret.setSize (nm);
        lapack_int const lda = n;
        char const uplo = 'U'; // FIXME: We store the whole matrix, which is unnecessary.
        // If only a part is stored, this parameter needs to reflect
        // which.
        bool ok = goMath::Lapack::TypeDriver<goFloat>::heev(matrix_type::rowMajor ? LAPACK_ROW_MAJOR : LAPACK_COL_MAJOR,
            jobz, uplo, n, a, lda, eigenvalues_ret.getPtr());
        if (!ok)
        {
            goLog::warning ("heev(): ierr != 0.");
            delete[] a;
            return false;
        }

        //= Copy the eigenvectors.
        if (eigenvectors_ret)
        {
            eigenvectors_ret->resize (nm);
            for (lapack_int i = 0; i < nm; ++i)
            {
                (*eigenvectors_ret)[i].resize (nm);
                for (int j = 0; j < nm; ++j)
                {
                    // FIXME: Are the eigenvectors actually stored in /a/ and
                    // are they stored row-major?
                    (*eigenvectors_ret)[i][j].re() = lapack_complex_float_real(a[i + j * nm]);
                    (*eigenvectors_ret)[i][j].im() = lapack_complex_float_imag(a[i + j * nm]);
                }
            }
        }

        delete[] a;
        return true;
    }


}; };
