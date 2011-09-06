/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>

template <class T>
bool goMath::planeLineCut (const goMath::Vector<T>& planeNormal, const goMath::Vector<T>& planePoint, 
                   const goMath::Vector<T>& lineDirection, const goMath::Vector<T>& linePoint,
                   goMath::Vector<T>& ret)
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
bool goMath::planeLineCut<goFloat> (const goMath::Vector<goFloat>& , const goMath::Vector<goFloat>& , 
                   const goMath::Vector<goFloat>& , const goMath::Vector<goFloat>& ,
                   goMath::Vector<goFloat>& ret);
template
bool goMath::planeLineCut<goDouble> (const goMath::Vector<goDouble>& , const goMath::Vector<goDouble>& , 
                   const goMath::Vector<goDouble>& , const goMath::Vector<goDouble>& ,
                   goMath::Vector<goDouble>& ret);
