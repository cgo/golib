#ifndef GOMATH_H
#define GOMATH_H

#include <math.h>

#ifndef MAX
#define MAX(_m1,_m2) (_m1 < _m2 ? _m2 : _m1)
#endif

#ifndef MIN
#define MIN(_m1,_m2) (_m1 < _m2 ? _m1 : _m2)
#endif

#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif

// The following functions are taken in part directly from the TNT library
// (http://math.nist.gov/tnt/). There is no copyright on TNT, but they ask
// for acknowledgment when TNT is used in a product.
namespace goMath
{
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
/*! @} */

bool gradient2D  (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool laplacian2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);

};


#endif
