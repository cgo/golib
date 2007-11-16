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
#ifndef GOAFFINETRANSFORM_H
# include <goaffinetransform.h>
#endif
#ifndef GOCUBICSPLINE_H
# include <gocubicspline.h>
#endif
#ifndef GORESAMPLE_H
# include <goresample.h>
#endif
#ifndef GOCONTOURS_H
# include <gocontours.h>
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
    /** 
     * @brief Modulus function.
     * 
     * @param value 
     * @param modulus 
     * 
     * @return \c value mod \c modulus
     */
    template <class T>
        T mod (T value, T modulus)
        {
            T m = value % modulus;
            if (m < 0)
                m = modulus + m;
            return m;
        }
/**
 * @}
 */

/**
 * \addtogroup math
 * @{
 */
/**
	@returns the absolute value of a real (no-complex) scalar.
*/
template <class Real>
Real abs(Real a)
{
	return  (a > 0 ? a : -a);
}
/**
 * @}
 */

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
		return goMath::abs(b);
	else
	{
		Real c = b/a;
		return Real(goMath::abs((double)a) * Real(sqrt(1.0 + (double)(c*c))));
	}
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
/*!
    ::acos seems to result in nan when the argument is exactly 1.0 (contrary to the manpage!).
    That is caught here.
	@return acos (a)
*/
template <class Real>
Real acos(const Real &a)
{
    if (a <= Real(-1.0))
    {
        return M_PI;
    }
    else
    {
        if (a >= Real(1.0))
        {
            return Real(0.0);
        }
        else
        {
            return ::acos (a);
        }
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
Scalar min(Scalar a, Scalar b)
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
Scalar max(Scalar a, Scalar b)
{
	return  a > b ? a : b;
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
template <class T>
T max (const goFixedArray<T>& a)
{
    goSize_t sz = a.getSize();
    if (sz < 1)
        return T(0);
    T ret = a[0];
    for (goSize_t i = 1; i < sz; ++i)
    {
        ret = goMath::max<T>(ret,a[i]);
    }
    return ret;
}
/*! @} */
/**
 * \addtogroup math
 * @{
 */
template <class T>
goSize_t maxIndex (const goFixedArray<T>& a)
{
    goSize_t sz = a.getSize();
    if (sz < 1)
        return 0;
    goSize_t ret = 0;
    for (goSize_t i = 1; i < sz; ++i)
    {
        if (a[i] > a[ret])
            ret = i;
    }
    return ret;
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
template <class T>
T min (const goFixedArray<T>& a)
{
    goSize_t sz = a.getSize();
    if (sz < 1)
        return T(0);
    T ret = a[0];
    for (goSize_t i = 1; i < sz; ++i)
    {
        ret = goMath::min<T>(ret,a[i]);
    }
    return ret;
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
template <class T>
goSize_t minIndex (const goFixedArray<T>& a)
{
    goSize_t sz = a.getSize();
    if (sz < 1)
        return 0;
    goSize_t ret = 0;
    for (goSize_t i = 1; i < sz; ++i)
    {
        if (a[i] < a[ret])
            ret = i;
    }
    return ret;
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
template <class T>
T maxabs (const goFixedArray<T>& a)
{
    goSize_t sz = a.getSize();
    if (sz < 1)
        return T(0);
    T ret = fabs(a[0]);
    for (goSize_t i = 1; i < sz; ++i)
    {
        ret = goMath::max<T>(ret,fabs(a[i]));
    }
    return ret;
}
/*! @} */

/**
 * \addtogroup math
 * @{
 */
template <class T>
T minabs (const goFixedArray<T>& a)
{
    goSize_t sz = a.getSize();
    if (sz < 1)
        return T(0);
    T ret = fabs(a[0]);
    for (goSize_t i = 1; i < sz; ++i)
    {
        ret = goMath::min<T>(ret,fabs(a[i]));
    }
    return ret;
}
/*! @} */

bool gradient2D          (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool laplacian2D         (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool ddx2D               (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool ddy2D               (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool divNormalizedGrad2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);

template <class T>
T stencil (const goSignal3DBase<void>& sig, const goMatrix<T>& s);

template <class T>
goSize_t getKnee (const goFixedArray<T>& x, const goFixedArray<T>& y);

/* 
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
bool divergence (const goSignal3DBase<void>& x, const goSignal3DBase<void>& y, goDouble hx, goDouble hy, goSignal3D<void>& retValue, const goSignal3DBase<void>* mask = 0);

/* 
 * @brief Central differences of a signal.
 * 
 * @param x Input signal.
 * @param retValue Central difference values.
 * @param dimension One of {0,1,2}
 * @param h Grid spacing.
 * @param mask Optional goInt8 mask.
 * 
 * @return True if successful, false otherwise.
 */
bool centralDifferences (const goSignal3DBase<void>& x, goSignal3DBase<void>& retValue, int dimension = 0, goDouble h = 1.0, const goSignal3DBase<void>* mask = 0);

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

template <class T>
bool centerOfMass (const goMatrix<T>& confMatrix, goVector<T>& comRet);

template <class pointT>
bool centerOfMass (const goList<pointT>&, pointT& comRet);

template <class pointT>
bool centerOfMass (typename goList<pointT>::ConstElement* begin, goIndex_t pointCount, pointT& comRet);

template <class T>
bool translate (goMatrix<T>& confMatrix, const goVector<T>& trans);

bool vectorMult (const goSignal3DBase<void>& V1, const goSignal3DBase<void>& V2, goSignal3DBase<void>& result);

template <class T>
bool affineMatch (const goMatrix<T>& X1, const goMatrix<T>& X2, goMatrix<T>& A, goVector<T>& t);

template <class T>
bool affineMatch(
        const goMatrix<T>& q,
        const goMatrix<T>& s,
        goDouble beta,
        const goMatrix<T>& q2,
        const goMatrix<T>& s2,
        goMatrix<T>& A,
        goVector<T>& t);

/** 
 * @brief Mean calculation.
 *
 * Slow implementation but prevents overflows.
 * 
 * @param v  Vector.
 * @param sz Number of elements in v.
 * 
 * @return Mean.
 */
template <class vectorT, class T>
T mean (const vectorT& v, goSize_t sz)
{
    goSize_t i;
    goDouble accum = 0.0;
    goDouble f = 1.0;
    for (i = 0; i < sz; ++i, f += 1.0)
    {
        accum = (accum * i + v[i]) / f;
    }
    return static_cast<T>(accum);
}

/** 
 * @brief Mean calculation.
 *
 * Fast implementation but can overflow (values are added, then divided once).
 * 
 * @param v  Vector.
 * @param sz Number of elements in v.
 * 
 * @return Mean.
 */
template <class vectorT, class T>
T fastMean (const vectorT& v, goSize_t sz)
{
    goSize_t i;
    goDouble accum = 0.0;
    for (i = 0; i < sz; ++i)
    {
        accum += v[i];
    }
    return static_cast<T>(accum / static_cast<goDouble>(sz));
}
template <class vectorT, class T>
T variance (const vectorT& v, goSize_t sz, T mean)
{
    goDouble accum = 0.0;
    goSize_t i;
    goDouble temp;
    goDouble f = 1.0;
    for (i = 0; i < sz; ++i, f += 1.0)
    {
        temp = v[i] - mean;
        accum = (accum * T(i) + temp * temp) / f;
    }
    return static_cast<T>(accum);
}

template <class vectorT, class T>
T fastVariance (const vectorT& v, goSize_t sz, T mean)
{
    goDouble accum = 0.0;
    goSize_t i;
    goDouble temp;
    for (i = 0; i < sz; ++i)
    {
        temp = v[i] - mean;
        accum += temp * temp;
    }
    return static_cast<T>(accum / static_cast<goDouble>(sz - 1));
}

template <class T>
void sin (goFixedArray<T>& a);
template <class T>
void cos (goFixedArray<T>& a);
template <class T>
void tan (goFixedArray<T>& a);
template <class T>
void asin(goFixedArray<T>& a);
template <class T>
void acos (goFixedArray<T>& a);
template <class T>
void atan (goFixedArray<T>& a);
template <class T>
void exp (goFixedArray<T>& a);
template <class T>
void log (goFixedArray<T>& a);

template <class vectorT, class T>
void diff (const vectorT& v, vectorT& ret, goSize_t sz, bool periodic = false)
{
    if (sz < 2)
    {
        return;
    }
    for (goSize_t i = 1; i < sz - 1; ++i)
    {
        ret[i] = (v[i + 1] - v[i - 1]) * 0.5;
    }
    if (periodic)
    {
        ret[0] = (v[1] - v[sz - 1]) * 0.5f;
        ret[sz - 1] = (v[0] - v[sz - 2]) * 0.5f;
    }
    else
    {
        ret[0] = 0.0;
        ret[sz - 1] = 0.0;
    }
}

/** 
 * @brief Student t density function.
 * 
 * \f$ f(x) = \frac{\Gamma((n+1)/2)}{\sqrt{\pi \cdot n} \cdot \Gamma(n/2)} \cdot \left( 1 + x^2/n \right)^{-(n+1)/2} \f$
 *
 * @param x Parameter x.
 * @param n Degrees of freedom.
 * 
 * @return The value of f(x) as defined above.
 */
template <class T>
T studentT (T x, goSize_t n)
{
    goDouble dn = (goDouble)n;
    return ::tgamma((dn + 1.0f) * 0.5f) / (sqrt(dn * M_PI) * ::tgamma (dn * 0.5)) * ::pow(1.0 + x*x / dn, -(dn+1.0)*0.5);
}

template <class vectorT, class T>
T integrate (const vectorT& x, const vectorT& y, goSize_t sz);
template <class vectorT, class T>
T integrate (const vectorT& x, const vectorT& y, vectorT& ret, goSize_t sz);

template <class T>
T integrate (const goVector<T>& v);
template <class T>
T integrateSum (const goVector<T>& v);

template <class T>
T integrateSimpson (const goVector<T>& v);

template <class T>
bool euclideanToBarycentric (const goMatrix<T>& simplex, const goVector<T>& point, goVector<T>& ret);

template <class T>
void barycentricToEuclidean (const goMatrix<T>& simplex, const goVector<T>& barycentric, goVector<T>& ret);

template <class T>
bool planeLineCut (const goVector<T>& planeNormal, const goVector<T>& planePoint, 
                   const goVector<T>& lineDirection, const goVector<T>& linePoint,
                   goVector<T>& ret);

template <class vectorT, class T>
bool getRoots (const vectorT& fX, 
               const vectorT& fY,
               vectorT&       retX,
               goVector<goIndex_t>* retRootIndex = 0);

template <class T>
bool pairwiseDistances (const goMatrix<T>& X, int dimension, goMatrix<T>& ret);
template <class T>
bool pdist (const goMatrix<T>& X, int dimension, goMatrix<T>& ret)
{
    return goMath::pairwiseDistances<T> (X, dimension, ret);
};

template <class T>
bool sphereToEuclidean (T phi, T theta, T radius,
                        goVector<T>* positionRet, goVector<T>* upRet);

template <class T>
bool euclideanToSphere (const goVector<T>& x, T& phiRet, T& thetaRet, T& radiusRet);

template <class T>
bool euclideanToSphere (const goVector<T>& x, goVector<T>& phitheta, T* radiusRet = 0);

template <class T>
bool sampleSphere (T dist, T radius, goList<goVector<T> >& positionRet, goList<goVector<T> >& upRet);
template <class T>
bool sampleSphere (T dist, T radius, goMatrix<T>& sphereRet);

/*! @} */
};


#endif
