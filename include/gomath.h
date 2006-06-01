#ifndef GOMATH_H
#define GOMATH_H

#include <math.h>
#ifndef GOVECTOR_H
# include <govector.h>
#endif

#ifndef MAX
#define MAX(_m1,_m2) (_m1 < _m2 ? _m2 : _m1)
#endif

#ifndef MIN
#define MIN(_m1,_m2) (_m1 < _m2 ? _m1 : _m2)
#endif

#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif

// The following functions are taken in part directly from the TNT library
// (http://math.nist.gov/tnt/). There is no copyright on TNT, but they ask
// for acknowledgment when TNT is used in a product.
namespace goMath
{
    extern const goFloat epsilon;
    
/**
 * \addtogroup math
 * @{
 */
/*!
	@returns hypotenuse of real (non-complex) scalars a and b by 
	avoiding underflow/overflow
	using (a * sqrt( 1 + (b/a) * (b/a))), rather than
	sqrt(a*a + b*b).
*/
template <class Real>
Real hypot(const Real &a, const Real &b)
{
	
	if (a == 0)
		return abs(b);
	else
	{
		Real c = b/a;
		return abs(a) * sqrt(1 + c*c);
	}
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
/**
	@returns the minimum of scalars a and b.
*/
template <class Scalar>
Scalar min(const Scalar &a, const Scalar &b)
{
	return  a < b ? a : b;
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
/**
	@returns the maximum of scalars a and b.
*/
template <class Scalar>
Scalar max(const Scalar &a, const Scalar &b)
{
	return  a > b ? a : b;
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
/**
	@returns the absolute value of a real (no-complex) scalar.
*/
template <class Real>
Real abs(const Real &a)
{
	return  (a > 0 ? a : -a);
}

bool gradient2D          (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool laplacian2D         (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool ddx2D               (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool ddy2D               (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool divNormalizedGrad2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);

/** 
 * @brief Divergence of a 2D vector field.
 * 
 * @param x x component.
 * @param y y component.
 * @param hx x grid size.
 * @param hy y grid size.
 * @param retValue divergence field.
 * 
 * @return True if successful, false otherwise.
 */
bool divergence (const goSignal3DBase<void>& x, const goSignal3DBase<void>& y, goDouble hx, goDouble hy, goSignal3D<void>& retValue);

/** 
 * @brief Central differences of a signal.
 * 
 * @param x Input signal.
 * @param retValue Central difference values.
 * @param dimension One of {0,1,2}
 * @param h Grid spacing.
 * 
 * @return True if successful, false otherwise.
 */
bool centralDifferences (const goSignal3DBase<void>& x, goSignal3D<void>& retValue, int dimension = 0, goDouble h = 1.0);

/** 
 * @brief Conjugate gradients solver.
 * 
 * Solves A*x=b iteratively for x.
 *
 * @note Currently implemented for goSparseMatrix and goVector.
 * 
 * @param A Matrix A
 * @param b Vector b
 * @param x Solution
 * @param epsilon Stop when abs(Ax-b) < epsilon.
 * 
 * @return final abs(A*x-b).
 */
template <class MatrixType, class VectorType>
goDouble goConjugateGradients (const MatrixType& A, const VectorType& b, VectorType& x, goDouble epsilon = 1e-6);

template <class T>
bool centerOfMass (const goList<goVector<T> >&, goVector<T>& comRet);

template <class pointT>
bool centerOfMass (const goList<pointT>&, pointT& comRet);

template <class pointT>
bool centerOfMass (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, pointT& comRet);

bool vectorMult (const goSignal3DBase<void>& V1, const goSignal3DBase<void>& V2, goSignal3DBase<void>& result);

template <class vectorT, class T>
T mean (const vectorT&, goSize_t sz);
template <class vectorT, class T>
T fastMean (const vectorT&, goSize_t sz);

template <class vectorT, class T>
T variance (const vectorT&, goSize_t sz, T mean);
template <class vectorT, class T>
T fastVariance (const vectorT&, goSize_t sz, T mean);

template <class vectorT, class T>
T integrate (const vectorT& x, const vectorT& y, goSize_t sz);
template <class vectorT, class T>
T integrate (const vectorT& x, const vectorT& y, vectorT& ret, goSize_t sz);

template <class vectorT, class T>
bool getRoots (const vectorT& fX, 
               const vectorT& fY,
               vectorT&       retX,
               goVector<goIndex_t>* retRootIndex = 0);
/*! @} */
};


#endif
