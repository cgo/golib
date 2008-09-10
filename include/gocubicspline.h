#ifndef GOCUBICSPLINE_H
#define GOCUBICSPLINE_H

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
        class CubicSpline
        {
            public:
                CubicSpline ();
                CubicSpline (const goMath::Matrix<T>& points);
                CubicSpline (const goMath::Vector<T>& pm1,
                             const goMath::Vector<T>& p0,
                             const goMath::Vector<T>& p1,
                             const goMath::Vector<T>& p2);
                virtual ~CubicSpline();
                
                goAutoPtr<goMath::Vector<T> > operator() (T t) const; //= eval()
                bool eval (T t, goMath::Vector<T>& ret) const;
                bool D (T t, goMath::Vector<T>& ret) const;
                T integrate (T t1, T t2);
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

typedef goMath::CubicSpline<goFloat> goCubicSplinef;
typedef goMath::CubicSpline<goDouble> goCubicSplined;

#endif
