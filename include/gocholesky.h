/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOCHOLESKY_H
#define GOCHOLESKY_H

#include <gomatrix.h>
#include <goarray.h>
#include <math.h>

namespace goMath
{

/*
 * Apparently taken out.
 */

/* 
 * \addtogroup mathla
 * @{
 */
/*
 * \internal
 * \brief Cholesky factorization
 *
   <P>
   Taken in large parts from the JAMA library
   (http://math.nist.org/tnt) which is not under copyright.

   For a symmetric, positive definite matrix A, this function
   computes the Cholesky factorization, i.e. it computes a lower 
   triangular matrix L such that A = L*L'.
   If the matrix is not symmetric or positive definite, the function
   computes only a partial decomposition.  This can be tested with
   the is_spd() flag.

   <p>Typical usage looks like:
   <pre>
	goMath::Matrix<double> A(n,n);
	goMath::Matrix<double> L;

	 ... 

	Cholesky<double> chol(A);

	if (chol.is_spd())
		L = chol.getL();
		
  	else
		cout << "factorization was not complete.\n";

	</pre>


   <p>
	(Adapted from JAMA, a Java Matrix Library, developed by jointly 
	by the Mathworks and NIST; see  http://math.nist.gov/javanumerics/jama).

   */
template <class Real>
class Cholesky
{
    public:
        Cholesky ();
        Cholesky (const goMath::Matrix<Real> &A);
        const goMath::Matrix<Real>& getL   () const;
        bool  solve  (const goArray<Real> &B, goArray<Real>& xRet);
        bool  solve  (const goMath::Matrix<Real> &B, goMath::Matrix<Real>& XRet);
        int            is_spd () const;

    protected:
        goMath::Matrix<Real> L_;		// lower triangular factor
        int isspd;				// 1 if matrix to be factored was SPD

};
/* @} */

}

#endif
