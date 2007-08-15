#include <gomath.h>

/** 
 * @brief Calculate the cut point between a line and a plane.
 * 
 * The plane is given as \f$ P = \{x \, : \, \langle n,(x-p_0) \rangle = 0 \} \f$,
 * with n the plane's normal vector and p0 a point on the plane.
 * The line is given as \f$ L(t) = d \cdot t + p_1 \f$
 * with d the direction vector of the line, t a parameter, and p1 a point on the line.
 *
 * @param planeNormal    Normal of the plane.
 * @param planePoint     A point in the plane.
 * @param lineDirection  Direction vector of the line.
 * @param linePoint      Point on the line.
 * @param ret            The cut point, if any.
 * 
 * @return True if a cut point was calculated, false otherwise.
 */
template <class T>
bool goMath::planeLineCut (const goVector<T>& planeNormal, const goVector<T>& planePoint, 
                   const goVector<T>& lineDirection, const goVector<T>& linePoint,
                   goVector<T>& ret)
{
    goDouble A = planeNormal * planePoint - planeNormal * linePoint;
    goDouble B = planeNormal * lineDirection;
    if (fabs(B) < 1e-6)
    {
        return false;
    }
    goDouble t = A / B;

    ret = linePoint + lineDirection * t;

    return true;
}

template
bool goMath::planeLineCut<goFloat> (const goVector<goFloat>& , const goVector<goFloat>& , 
                   const goVector<goFloat>& , const goVector<goFloat>& ,
                   goVector<goFloat>& ret);
template
bool goMath::planeLineCut<goDouble> (const goVector<goDouble>& , const goVector<goDouble>& , 
                   const goVector<goDouble>& , const goVector<goDouble>& ,
                   goVector<goDouble>& ret);
