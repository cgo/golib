
#ifndef GOPLOT_OBJECT2DBOX_H
#define GOPLOT_OBJECT2DBOX_H

#include <goplot/plot.h>

#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goPlot
{

    /** @addtogroup cairoplot
     * @{
     */
    /** 
     * @brief Box object. Draws a box.
     */
    class Object2DBox : public Object2D
    {
        public:
            Object2DBox (goDouble x1 = 0.0, goDouble y1 = 0.0, goDouble x2 = 0.0, goDouble y2 = 0.0);

            virtual ~Object2DBox ();

            void setCorners (goDouble x1, goDouble y1, goDouble x2, goDouble y2);
            void setCorners (const goMatrixd&);
            const goMatrixd& getCorners () const;

            LineTraits& lineTraits ();
            const LineTraits& lineTraits () const;

            virtual void draw ();

        private:
            goMatrixd  myCorners;
            LineTraits myLineTraits;
    };
    /** @} */
};

#endif
