/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOLU_H
#define GOLU_H

#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
//#ifndef GOMATRIX_HPP
//# include <gomatrix.hpp>
//#endif
#ifndef GOARRAY_H
# include <goarray.h>
#endif
#include <gomath.h>

namespace goMath 
{


/**
 * \addtogroup mathla
 * @{
 */
    
   /** 
    * \internal
    * \brief LU Decomposition.
    * 
    * Taken in large parts from the JAMA library (free of copyright).
    *
   <P>
   For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
   unit lower triangular matrix L, an n-by-n upper triangular matrix U,
   and a permutation vector piv of length m so that A(piv,:) = L*U.
   If m < n, then L is m-by-m and U is m-by-n.
   <P>
   The LU decompostion with pivoting always exists, even if the matrix is
   singular, so the constructor will never fail.  The primary use of the
   LU decomposition is in the solution of square systems of simultaneous
   linear equations.  This will fail if isNonsingular() returns false.
   */
template <class Real>
class LU
{



   /* Array for internal storage of decomposition.  */
   goMath::Matrix<Real>  LU_;
   int m, n, pivsign; 
   goMath::Vector<int> piv;


   void permute_copy(const goMath::Matrix<Real> &A, 
   			const goMath::Vector<int> &piv, int j0, int j1, goMath::Matrix<Real>& retValue)
	{
		int piv_length = piv.getSize();

        retValue.resize (piv_length, j1-j0+1);
		// goMath::Matrix<Real> X(piv_length, j1-j0+1);

         for (int i = 0; i < piv_length; i++) 
            for (int j = j0; j <= j1; j++) 
               retValue(i,j-j0) = A(piv[i],j);
		// return X;
	}

   void permute_copy(const goMath::Vector<Real> &A, 
   		const goMath::Vector<int> &piv, goMath::Vector<Real>& retValue)
	{
		int piv_length = piv.getSize();
		if (piv_length != (int)A.getSize())
			 retValue.resize(0);

        if ((int)retValue.getSize() != piv_length)
            retValue.resize (piv_length);
		// goMath::Vector<Real> x(piv_length);

         for (int i = 0; i < piv_length; i++) 
               retValue[i] = A[piv[i]];

		// return x;
	}


	public :

   /** LU Decomposition
   @param  A   Rectangular matrix
   @return     LU Decomposition object to access L, U and piv.
   */

    //goLU (const goMath::Matrix<Real> &A) : LU_(A.copy()), m(A.dim1()), n(A.dim2()), 
	//	piv(A.dim1())
    LU (const goMath::Matrix<Real> &A) : LU_(A), m(A.dim1()), n(A.dim2()), 
		piv(A.dim1())
	
	{

   // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

	  int i=0;
	  int j=0;
	  int k=0;

      for (i = 0; i < m; i++) {
         piv[i] = i;
      }
      pivsign = 1;
      goMath::Vector<Real> LUcolj(m);

      // Outer loop.

      for (j = 0; j < n; j++) 
      {
         // Make a copy of the j-th column to localize references.
         for (i = 0; i < m; i++) {
            LUcolj[i] = LU_(i,j);
         }

         // Apply previous transformations.
         for (i = 0; i < m; i++) {
            goMath::Vector<Real> LUrowi;
            LU_.refRow(i, LUrowi);

            // Most of the time is spent in the following dot product.
            int kmax = goMath::min(i,j);
            double s = 0.0;
            for (k = 0; k < kmax; k++) {
               s += LUrowi[k]*LUcolj[k];
            }

            LUrowi[j] = LUcolj[i] -= Real(s);
         }

         // Find pivot and exchange if necessary.
         int p = j;
         for (i = j+1; i < m; i++) {
            if (abs(LUcolj[i]) > abs(LUcolj[p])) {
               p = i;
            }
         }
         if (p != j) {
            for (k = 0; k < n; k++) {
               double t = LU_(p,k); 
			   LU_(p,k) = LU_(j,k); 
			   LU_(j,k) = Real(t);
            }
            k = piv[p]; 
			piv[p] = piv[j]; 
			piv[j] = k;
            pivsign = -pivsign;
         }

         // Compute multipliers.
         
         if ((j < m) && (LU_(j,j) != 0.0)) 
         {
            for (i = j+1; i < m; i++) 
            {
               LU_(i,j) /= LU_(j,j);
            }
         }
      }
   }


   /** Is the matrix nonsingular?
   @return     1 (true)  if upper triangular factor U (and hence A) 
   				is nonsingular, 0 otherwise.
   */

   int isNonsingular () {
      for (int j = 0; j < n; j++) {
         if (LU_(j,j) == 0)
            return 0;
      }
      return 1;
   }

   /** Return lower triangular factor
   @return     L
   */

   goMath::Matrix<Real> getL () {
      goMath::Matrix<Real> L_(m,n);
      for (int i = 0; i < m; i++) {
         for (int j = 0; j < n; j++) {
            if (i > j) {
               L_(i,j) = LU_(i,j);
            } else if (i == j) {
               L_(i,j) = 1.0;
            } else {
               L_(i,j) = 0.0;
            }
         }
      }
      return L_;
   }

   /** Return upper triangular factor
   @return     U portion of LU factorization.
   */

   goMath::Matrix<Real> getU () {
   	  goMath::Matrix<Real> U_(n,n);
      for (int i = 0; i < n; i++) {
         for (int j = 0; j < n; j++) {
            if (i <= j) {
               U_(i,j) = LU_(i,j);
            } else {
               U_(i,j) = 0.0;
            }
         }
      }
      return U_;
   }

   /** Return pivot permutation vector
   @return     piv
   */

   goMath::Vector<int> getPivot () {
      return piv;
   }


   /** Compute determinant using LU factors.
   @return     determinant of A, or 0 if A is not square.
   */

   Real det () {
      if (m != n) {
         return Real(0);
      }
      Real d = Real(pivsign);
      for (int j = 0; j < n; j++) {
         d *= LU_(j,j);
      }
      return d;
   }

   /** Solve A*X = B
   @param  B   A Matrix with as many rows as A and any number of columns.
   @return     X so that L*U*X = B(piv,:), if B is nonconformant, returns
   					0x0 (null) array.
   */

   bool solve (const goMath::Matrix<Real> &B, goMath::Matrix<Real>& retValue) 
   {

	  /* Dimensions: A is mxn, X is nxk, B is mxk */
      
      if (B.dim1() != m) {
	  	return false;
      }
      if (!isNonsingular()) {
        return false;
      }

      // Copy right hand side with pivoting
      int nx = B.dim2();

      permute_copy(B, piv, 0, nx-1, retValue);

      // Solve L*Y = B(piv,:)
      for (int k = 0; k < n; k++) {
         for (int i = k+1; i < n; i++) {
            for (int j = 0; j < nx; j++) {
               retValue(i,j) -= retValue(k,j)*LU_(i,k);
            }
         }
      }
      // Solve U*X = Y;
      for (int k = n-1; k >= 0; k--) {
         for (int j = 0; j < nx; j++) {
            retValue(k,j) /= LU_(k,k);
         }
         for (int i = 0; i < k; i++) {
            for (int j = 0; j < nx; j++) {
               retValue(i,j) -= retValue(k,j)*LU_(i,k);
            }
         }
      }
      return true;
   }


   /** Solve A*x = b, where x and b are vectors of length equal	
   		to the number of rows in A.

   @param  b   a vector (goMath::Vector> of length equal to the first dimension
   						of A.
   @return x a vector (goMath::Vector> so that L*U*x = b(piv), if B is nonconformant,
   					returns 0x0 (null) array.
   */
   bool solve (const goMath::Vector<Real> &b, goMath::Vector<Real>& retValue) 
   {

	  /* Dimensions: A is mxn, X is nxk, B is mxk */
      
      if (b.getSize() != (goSize_t)m) {
	  	return false;
      }
      if (!isNonsingular()) {
        return false;
      }

      permute_copy(b, this->piv, retValue);

      // Solve L*Y = B(piv)
      for (int k = 0; k < n; k++) {
         for (int i = k+1; i < n; i++) {
               retValue[i] -= retValue[k]*LU_(i,k);
            }
         }
      
	  // Solve U*X = Y;
      for (int k = n-1; k >= 0; k--) {
            retValue[k] /= LU_(k,k);
      		for (int i = 0; i < k; i++) 
            	retValue[i] -= retValue[k]*LU_(i,k);
      }
      return true;
   }

}; /* class LU */

/*! @} */

} /* namespace JAMA */

#endif
/* JAMA_LU_H */
