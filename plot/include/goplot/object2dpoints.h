#ifndef GOPLOT_OBJECT2DPOINTS_H
#define GOPLOT_OBJECT2DPOINTS_H

#include <goplot/plot.h>
#include <cairo/cairo.h>

namespace NSPACE
{

    //= Cairo 2D drawing object
    //= Points must be derived from Points2D.
    /** 
     * @brief Points object. Draws lines connecting the given 2D points.
     * @param Points Class derived from Points2D.
     * @param Real Real type (float or double)
     */
    template <class Points, class Real>
    class Object2DPoints : public Object2D
    {
        public:
            Object2DPoints (size_t N = 1) 
                : myPoints (N), 
                  myLineTraits () 
            {
                myLineTraits.setWidth (1.0);
            }

            virtual ~Object2DPoints () { };

            Points& points () { return myPoints; };
            const Points& points () const { return myPoints; };

            LineTraits& lineTraits () { return myLineTraits; };
            const LineTraits& lineTraits () const { return myLineTraits; };

            virtual void draw ()
            {
                if (myPoints.size () < 1)
                {
                    return;
                }

                cairo_t* cr = this->context ();
                if (!cr)
                    return;

                cairo_save (cr);

                myLineTraits.apply (cr);
                this->applyTransform (cr);

                cairo_move_to (cr, myPoints.x (0), myPoints.y (0));
                for (size_t i = 1; i < myPoints.size (); ++i)
                {
                    cairo_line_to (cr, myPoints.x (i), myPoints.y (i));
                }
                //= Set to identity matrix before stroke, so that line width is in device coordinates (e.g. pixels)
                cairo_identity_matrix (cr);
                cairo_stroke (cr);

                //= Halbtransparent fuellen ...
                //cairo_stroke_preserve (cr);
                //RGBA c = lineTraits().colour ();
                //c.a = 0.2;
                //lineTraits().setColour (c);
                //lineTraits().apply (cr);
                //cairo_fill (cr);

                cairo_restore (cr);
            };

        private:
            Points         myPoints;
            LineTraits     myLineTraits;
    };
};

#endif
