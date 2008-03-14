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
                CubicSplineND (const goMatrix<T>& points);
                CubicSplineND (const goVector<T>& pm1,
                             const goVector<T>& p0,
                             const goVector<T>& p1,
                             const goVector<T>& p2);
                virtual ~CubicSplineND();
                
                goAutoPtr<goVector<T> > operator() (T t) const; //= eval()
                bool eval (T t, goVector<T>& ret) const;
                bool D (T t, goVector<T>& ret) const;
                bool fit (const goMatrix<T>& points);
                bool fit (const goVector<T>& pm1,
                          const goVector<T>& p0,
                          const goVector<T>& p1,
                          const goVector<T>& p2);
            private:
                goMatrix<T> myA_inv;
                goMatrix<T> myM;
        };
        /** @} */
};

typedef goMath::CubicSplineND<goFloat> goCubicSplineNDf;
typedef goMath::CubicSplineND<goDouble> goCubicSplineNDd;

#endif
