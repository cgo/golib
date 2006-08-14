#include <goeigenvalue.h>
#include <golog.h>

//= From SEISPACK
#include <f2c.h>
extern "C" int ch_(integer *nm, integer *n, real *ar, real *ai, real *w,
       integer *matz, real *zr, real *zi, real *fv1, real *fv2, real *fm1,
      integer *ierr);

/**
 * @brief 
 *
 * @todo Fix the sizes of the temporary arrays for ch_ or use a different implementation.
 *
 * @return True if successful, false otherwise.
 **/
bool goMath::goComplexEigenvaluesHermite (const goMatrix<goComplexf>&          m, 
                                          goVectorf&                           eigenvaluesRet, 
                                          goFixedArray< goVector<goComplexf> >* eigenvectorsRet)
{
    if (m.getColumns() != m.getRows())
        return false;

    integer n = m.getColumns();
    integer nm = n;
    integer matz = eigenvectorsRet ? 1 : 0;
    real* ar = new real [nm * nm];
    real* ai = new real [nm * nm];
    //= I suppose we store in row-major.
    // const goComplexf* matrix = m.getData();
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
            }
        }
    }
    eigenvaluesRet.setSize (n);
    real* zr = 0;
    real* zi = 0;
    if (eigenvectorsRet)
    {
        zr = new real [nm * n];
        zi = new real [nm * n];
    }
    // FIXME: How large do these have to be??
    real* fv1 = new real [nm * n];
    real* fv2 = new real [nm * n];
    real* fm1 = new real [nm * n];
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
    //= Copy the eigenvectors.
    if (eigenvectorsRet)
    {
        eigenvectorsRet->setSize (n);
        ar_p = zr;
        ai_p = zi;
        goIndex_t i;
        for (i = 0; i < n; ++i)
        {
            (*eigenvectorsRet)[i].setSize (nm);
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
    return true;
}
