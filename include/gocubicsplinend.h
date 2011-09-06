/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOCUBICSPLINEND_H
#define GOCUBICSPLINEND_H

#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goMath
{
    /** \addtogroup math
    * @{
    */

    /** 
    * @brief Cubic interpolating splines.
    */
    template <class T>
        class CubicSplineND
        {
            public:
                CubicSplineND ();
                CubicSplineND (const goMath::Matrix<T>& points);
                CubicSplineND (const goMath::Vector<T>& pm1,
                             const goMath::Vector<T>& p0,
                             const goMath::Vector<T>& p1,
                             const goMath::Vector<T>& p2);
                virtual ~CubicSplineND();
                
                goAutoPtr<goMath::Vector<T> > operator() (T t) const; //= eval()
                bool eval (T t, goMath::Vector<T>& ret) const;
                bool D (T t, goMath::Vector<T>& ret) const;
                bool fit (const goMath::Matrix<T>& points);
                bool fit (const goMath::Vector<T>& pm1,
                          const goMath::Vector<T>& p0,
                          const goMath::Vector<T>& p1,
                          const goMath::Vector<T>& p2);
            private:
                goMath::Matrix<T> myA_inv;
                goMath::Matrix<T> myM;
        };
        /** @} */
};

typedef goMath::CubicSplineND<goFloat> goCubicSplineNDf;
typedef goMath::CubicSplineND<goDouble> goCubicSplineNDd;

#endif
