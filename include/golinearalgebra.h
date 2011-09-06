/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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

