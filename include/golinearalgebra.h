#ifndef GO_LINEAR_ALGEBRA_H
#define GO_LINEAR_ALGEBRA_H

#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#ifndef GOARRAY_H
# include <goarray.h>
#endif

namespace goMath {

/*
 * Contains NRC code. No public use.
 */
template<class T>
void LUDecomp(goMatrix<T>& a, goArray<int>& indx, double *d);

/*
 * Contains NRC code. No public use.
 */
template<class T>
void LUBackSubst(goMatrix<T>& a, goArray<int>& indx, goArray<T>& b);

/*
 * Contains NRC code. No public use.
 * Inverts matrix a using LUDecomp and LUBackSubst (LU decomposition and backsubstitution).
 */
template<class T>
void matrixInversion(goMatrix<T>& a);

};

#endif

