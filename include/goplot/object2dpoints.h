#ifndef GOPLOT_OBJECT2DPOINTS_H
#define GOPLOT_OBJECT2DPOINTS_H

#include <goplot/plot.h>

#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goPlot
{

    //= Cairo 2D drawing object
    //= Points must be derived from Points2D.
    /** @addtogroup cairoplot
     * @{
     */
    /** 
     * @brief Points object. Draws lines connecting the given 2D points.
     * @param Points Class derived from Points2D.
     * @param Real Real type (float or double)
     */
    // template <class Points, class Real>
    class Object2DPoints : public Object2D
    {
        public:
            Object2DPoints (size_t N = 1);

            Object2DPoints (goAutoPtr<Points2D> p);

            void setPoints (goAutoPtr<Points2D> p);

            virtual ~Object2DPoints ();

            Points2D& points ();
            const Points2D& points () const;

            LineTraits& lineTraits ();
            const LineTraits& lineTraits () const;

            virtual void draw ();

        private:
            goAutoPtr<Points2D> myPoints;
            LineTraits     myLineTraits;
    };
    /** @} */
};

#endif
