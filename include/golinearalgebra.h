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
void LUDecomp(goMath::Matrix<T>& a, goArray<int>& indx, double *d);

/*
 * Contains NRC code. No public use.
 */
template<class T>
void LUBackSubst(goMath::Matrix<T>& a, goArray<int>& indx, goArray<T>& b);

/*
 * Contains NRC code. No public use.
 * Inverts matrix a using LUDecomp and LUBackSubst (LU decomposition and backsubstitution).
 */
template<class T>
void matrixInversion(goMath::Matrix<T>& a);

};

#endif

