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

    template <class T>
        class SVD
        {
            private:
                goMatrix<T> U;
                goMatrix<T> V;
                goVector<T> s;
                goSize_t    myM;
                goSize_t    myN;

            public:
                SVD (const goMatrix<T>& A, bool thin = true);
                virtual ~SVD ();

                goMatrix<T>&       getU ();
                const goMatrix<T>& getU () const;
                goMatrix<T>&       getV ();
                const goMatrix<T>& getV () const;
                goVector<T>&       getSingularValues ();
                const goVector<T>& getSingularValues () const;
                void getS (goMatrix<T>& S) const;

                bool calculate (const goMatrix<T>& A, bool thin = true);
        };

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
	goMatrix<Real> U, V;
	goVector<Real> s;
	int m, n;

  public:
   ThinSVD (const goMatrix<Real> &Arg);
   virtual ~ThinSVD ();


   void getU (goMatrix<Real> &A);

   /* Return the right singular vectors */
   void getV (goMatrix<Real> &A);

   goMatrix<Real>& getU ();
   const goMatrix<Real>& getU () const;
   goMatrix<Real>& getV ();
   const goMatrix<Real>& getV () const;

   /** Return the one-dimensional array of singular values */
   void getSingularValues (goVector<Real> &x);

   goVector<Real>& getSingularValues ();
   const goVector<Real>& getSingularValues () const;

   /** Return the diagonal matrix of singular values
   @return     S
   */
   void getS (goMatrix<Real> &A);

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
