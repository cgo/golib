#include <gomath.h>

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
