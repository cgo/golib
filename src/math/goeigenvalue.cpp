/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goeigenvalue.h>
#include <golog.h>
#include <goconfig.h>

//= From SEISPACK
#if OSX
# define integer int
# define real float
#else
# include <f2c.h>
#endif
extern "C" int ch_(integer *nm, integer *n, real *ar, real *ai, real *w,
       integer *matz, real *zr, real *zi, real *fv1, real *fv2, real *fm1,
      integer *ierr);

/** \addtogoup math
 * @{
 */
/**
 * @brief Eigenvalues and Eigenvectors of a complex Hermitian matrix.
 * @todo Replace SEISPACK with LAPACKE routines.
 * @return Number of correct eigenvalues and eigenvectors the function was able to compute.
 **/
goSize_t goMath::complexEigenvaluesHermite (const goMath::Matrix<goComplexf>&          m, 
                                          Vector<goFloat>&                           eigenvaluesRet, 
                                          goFixedArray< Vector<goComplexf> >* eigenvectorsRet)
{
    if (m.getColumns() != m.getRows())
        return 0;

    integer n = m.getRows();
    integer nm = m.getRows();
    integer matz = eigenvectorsRet ? 1 : 0;
    real* ar = new real [nm * nm];
    real* ai = new real [nm * nm];
    //= I suppose we store in column-major (from Fortran code).
    real* ar_p = ar;
    real* ai_p = ai;
    {
        goSize_t i;
        goSize_t j;
        for (j = 0; j < m.getColumns(); ++j)
        {
            for (i = 0; i < m.getRows(); ++i)
            {
                *ar_p = m(i,j).re();
                *ai_p = m(i,j).im();
                ++ar_p;
                ++ai_p;
            }
        }
    }
    eigenvaluesRet.setSize (nm);
    real* zr = 0;
    real* zi = 0;
    if (eigenvectorsRet)
    {
        zr = new real [nm * nm];
        zi = new real [nm * nm];
    }
    // FIXME: How large do these have to be??
    real* fv1 = new real [n];
    real* fv2 = new real [n];
    real* fm1 = new real [2 * n];
    integer ierr = 0;
    ch_ (&nm, &n, ar, ai, eigenvaluesRet.getPtr(), &matz, zr, zi, fv1, fv2, fm1, &ierr);
    if (ierr != 0)
    {
        goLog::warning ("goComplexEigenvaluesHermite(): ierr != 0.");
    }
    delete[] fm1;
    delete[] fv2;
    delete[] fv1;
    delete[] ai;
    delete[] ar;

    goSize_t eigenvalueCount = nm;

    if (ierr != 0)
    {
        eigenvalueCount = ierr - 1;
    }
    if (eigenvalueCount == 0)
    {
        return 0;
    }
    //= Copy the eigenvectors.
    if (eigenvectorsRet)
    {
        eigenvectorsRet->setSize (eigenvalueCount);
        ar_p = zr;
        ai_p = zi;
        goSize_t i;
        for (i = 0; i < eigenvalueCount; ++i)
        {
            (*eigenvectorsRet)[i].resize (nm);
            goIndex_t j;
            for (j = 0; j < nm; ++j)
            {
                (*eigenvectorsRet)[i][j].re() = *ar_p;
                (*eigenvectorsRet)[i][j].im() = *ai_p;
                ++ar_p;
                ++ai_p;
            }
        }
    }
    delete[] zi;
    delete[] zr;
    return eigenvalueCount;
}

/** @} */
