//=
//= goSignal math functions
//=

#ifndef GOMATHSIGNAL_H
#define GOMATHSIGNAL_H

#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif
#ifndef GOMATH_H
# include <gomath.h>
#endif

namespace goMath {

//= Remove all signal3d dependencies from gomath
//
bool gradient2D          (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool laplacian2D         (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool ddx2D               (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool ddy2D               (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);
bool divNormalizedGrad2D (const goSignal3DBase<void>& sig, goSignal3DBase<void>& retValue);

template <class T>
T stencil (const goSignal3DBase<void>& sig, const goMath::Matrix<T>& s);

/* 
 * @brief 2D euclidean transformation of an image.
 * 
 * @param source 
 * @param scale 
 * @param angle 
 * @param t_x 
 * @param t_y 
 * @param target 
 */
void transform2D (
        const goSignal3DBase<void>& source, 
        goDouble scale,
        goDouble angle,
        goDouble t_x,
        goDouble t_y,
        goSignal3D<void>& target,
        bool setsize = true);

void scale2D (const goSignal3DBase<void>& source, goSignal3DBase<void>& target, bool keep_aspect = false);
void paste2D (const goSignal3DBase<void>& source, 
        goDouble scale,
        goDouble angle,
        goDouble t_x,
        goDouble t_y,
        goSignal3D<void>& target,
        goFloat bgColour);
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

bool forwardDifferences (const goSignal3DBase<void>& x, goSignal3DBase<void>& retValue, int dimension = 0, goDouble h = 1.0, const goSignal3DBase<void>* mask = 0);

bool backwardDifferences (const goSignal3DBase<void>& x, goSignal3DBase<void>& retValue, int dimension = 0, goDouble h = 1.0, const goSignal3DBase<void>* mask = 0);

bool curvatureDirect2D (const goSignal3DBase<void>& input, goSignal3D<void>& result, goDouble hx = 1.0, goDouble hy = 1.0);

bool vectorMult (const goSignal3DBase<void>& V1, const goSignal3DBase<void>& V2, goSignal3DBase<void>& result);

//template <class T>
//bool binaryImage (const goMath::Matrix<T>& boundary, goSignal3D<void>& ret, goSize_t width, goSize_t height);

};
#endif
