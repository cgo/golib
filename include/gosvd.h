/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSVD_H
#define GOSVD_H

#include <goarray.h>
#include <govector.h>
#include <gomath.h>
#include <gomatrix.h>
// #include <gomatrix.hpp>

#include <algorithm>
// for min(), max() below
#include <cmath>
// for abs() below

using namespace std;

namespace goMath
{

/** 
* @addtogroup mathla
* @{
*/
/**
* @brief Singular value decomposition.
* This class can do full SVD or thin SVD. It uses
* sgesvd_() or dgesvd_() from the linked LAPACK library (as opposed to 
* goMath::ThinSVD). Instantiated for goFloat and goDouble types.
*/
    template <class T>
        class SVD
        {
            private:
                goMath::Matrix<T> U;
                goMath::Matrix<T> V;
                goMath::Vector<T> s;
                goSize_t    myM;
                goSize_t    myN;

            public:
                SVD (const goMath::Matrix<T>& A, bool thin = true);
                virtual ~SVD ();

                goMath::Matrix<T>&       getU ();
                const goMath::Matrix<T>& getU () const;
                goMath::Matrix<T>&       getV ();
                const goMath::Matrix<T>& getV () const;
                goMath::Vector<T>&       getSingularValues ();
                const goMath::Vector<T>& getSingularValues () const;
                void getS (goMath::Matrix<T>& S) const;

                bool calculate (const goMath::Matrix<T>& A, bool thin = true);
        };
/** @} */

#ifdef min
# undef min
#endif
#ifdef max
# undef max
#endif
#ifdef abs
# undef abs
#endif

    /*! \addtogroup math
     * @{
     */
   /** Singular Value Decomposition.
   * This is taken from the JAMA toolkit. JAMA is free of copyright.
   * The classes are renamed to fit in the go... naming conventions.
   <P>
   For an m-by-n matrix A with m >= n, the singular value decomposition is
   an m-by-n orthogonal matrix U, an n-by-n diagonal matrix S, and
   an n-by-n orthogonal matrix V so that A = U*S*V'.
   <P>
   The singular values, sigma[k] = S(k,k), are ordered so that
   sigma[0] >= sigma[1] >= ... >= sigma[n-1].
   <P>
   The singular value decompostion always exists, so the constructor will
   never fail.  The matrix condition number and the effective numerical
   rank can be computed from this decomposition.

   <p>
	(Adapted from JAMA, a Java Matrix Library, developed by jointly 
	by the Mathworks and NIST; see  http://math.nist.gov/javanumerics/jama).
   */
template <class Real>
class ThinSVD 
{
	goMath::Matrix<Real> U, V;
	goMath::Vector<Real> s;
	int m, n;

  public:
   ThinSVD (const goMath::Matrix<Real> &Arg);
   virtual ~ThinSVD ();


   void getU (goMath::Matrix<Real> &A);

   /* Return the right singular vectors */
   void getV (goMath::Matrix<Real> &A);

   goMath::Matrix<Real>& getU ();
   const goMath::Matrix<Real>& getU () const;
   goMath::Matrix<Real>& getV ();
   const goMath::Matrix<Real>& getV () const;

   /** Return the one-dimensional array of singular values */
   void getSingularValues (goMath::Vector<Real> &x);

   goMath::Vector<Real>& getSingularValues ();
   const goMath::Vector<Real>& getSingularValues () const;

   /** Return the diagonal matrix of singular values
   @return     S
   */
   void getS (goMath::Matrix<Real> &A);

   /** Two norm  (max(S)) */
   double norm2 ();

   /** Two norm of condition number (max(S)/min(S)) */
   double cond ();

   /** Effective numerical matrix rank
   @return     Number of nonnegligible singular values.
   */
   int rank ();
};

/*!
 * @}
 */

}

#endif
