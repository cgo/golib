#ifndef PLOT_H
#define PLOT_H

#define NSPACE goPlot

#include <math.h>
#include <cairo.h>
#include <pango/pangocairo.h>

namespace NSPACE
{

    typedef double real;

    //= Surface --> Graphs --> draw2d

    // template <class Points, class Real> class Graph;

    /** 
     * @brief Base class for types which provide 2D points.
     * @param Real Real type (float or double)
     */
    template <class Real>
        class Points2D
        {
            public:
                Points2D () { };
                virtual ~Points2D () { };
               
                //! Reimplement these.
                //! Return i'th point x value
                virtual Real x (int i) const { return Real(0); }
                //! Return i'th point x value
                virtual Real y (int i) const { return Real(0); }
                //! Set point number i
                virtual void set (int i, Real x, Real y) { };
                //! Return number of points
                virtual size_t size () const { return 0; };
        };

    /** 
     * @brief Simple 2D points provider. Just stores the points in a C array.
     */
    template <class Real>
        class Points2DSimple : public Points2D <Real>
    {
        public:
            Points2DSimple (int count = 1)
                : Points2D<Real> (), myPoints (0), mySize (0), myDelete (true)
            {
                myPoints = new Real[count * 2];
                mySize = count;
            };

            Points2DSimple (Real* ptr, size_t size)
                : Points2D<Real> (), myPoints (ptr), mySize (size), myDelete (false)
            {
            };

            template <class T> Points2DSimple (const Points2D<T>& other)
                : Points2D<Real> (), myPoints (0), mySize (0), myDelete (true)
            {
                *this = other;
            };

            template <class T> Points2DSimple<Real>& operator= (const Points2D<T>& other)
            {
                if (myPoints && myDelete)
                {
                    delete[] myPoints;
                    myPoints = 0;
                }
                mySize = other.size ();
                myPoints = new Real [mySize * 2];
                myDelete = true;
                for (size_t i = 0; i < mySize; ++i)
                {
                    this->set (i, other.x (i), other.y (i));
                }

                return *this;
            };

            virtual ~Points2DSimple ()
            {
                if (myPoints && myDelete)
                {
                    delete[] myPoints;
                    myPoints = 0;
                    mySize = 0;
                }
            };

            virtual void set (int i, Real x, Real y) 
            { 
                Real* p = &myPoints[i * 2];
                p[0] = x;
                p[1] = y;
            };

            virtual Real x (int i) { return *(myPoints + i * 2); }
            virtual Real y (int i) { return *(myPoints + i * 2 + 1); }

            const Real* operator () (int i) const { return myPoints + i * 2; };
            virtual size_t size () const { return mySize; };

        private:
            Real*  myPoints;
            size_t mySize;
            bool   myDelete;
    };

    /** 
     * @brief Colour class with alpha channel.
     */
    class RGBA
    {
        public:
            RGBA (double r = 0.0, double g = 0.0, double b = 0.0, double a = 1.0)
                : r (r), g (g), b (b), a (a)
            {
            };

            double r, g, b, a;
    };

    /** 
     * @brief Simple transformation representation (2x2 matrix and translation vector).
     */
    template <class Real> class Trafo2D
    {
        public:
            //= Matrix:
            //=   xx xy
            //=   yx yy
            //= Plus a translation (x0, y0)^T
            Trafo2D (Real xx = 1.0, Real yx = 0.0, Real xy = 0.0, Real yy = 1.0, Real x0 = 0.0, Real y0 = 0.0)
                : xx (xx), xy (xy), yx (yx), yy (yy), x0 (x0), y0 (y0)
            {
            };

            Trafo2D (const Trafo2D<Real>& t)
            {
                *this = t;
            };
            Trafo2D<Real>& operator= (const Trafo2D<Real>& t)
            {
                xx = t.xx;
                xy = t.xy;
                yx = t.yx;
                yy = t.yy;
                x0 = t.x0;
                y0 = t.y0;

                return *this;
            };

            virtual ~Trafo2D () { };

            void print () const
            {
                printf ("%.4f %.4f | %.4f\n%.4f %.4f | %.4f\n", xx, xy, x0, yx, yy, y0);
            };

            void apply (cairo_t* cr)
            {
                cairo_matrix_t M;
                cairo_matrix_init (&M, xx, yx, xy, yy, x0, y0);
                cairo_transform (cr, &M);
            }

            Real xx, xy, yx, yy, x0, y0;
    };

    /** 
     * @brief Base class for all 2D drawable objects.
     */
    class Object2D
    {
        public:
            virtual ~Object2D () { };

            virtual void draw () = 0;
            virtual void setContext (cairo_t* c) { myContext = c; }

            // Trafo2D<real>& transform () { return myTransform; };
            const Trafo2D<real>& transform () const { return myTransform; };
            void setTransform (const Trafo2D<real>& T) { myTransform = T; };

            cairo_t* context () { return myContext; }
            const cairo_t* context () const { return myContext; }

            void applyTransform (cairo_t* cr) 
            {
                this->myTransform.apply (cr);
                //cairo_matrix_t M;
                //cairo_matrix_init (&M, myTransform.xx, myTransform.yx, myTransform.xy, myTransform.yy, myTransform.x0, myTransform.y0);
                //cairo_transform (cr, &M);

                //cairo_matrix_t Mo;
                //cairo_get_matrix (cr, &Mo);
                //cairo_matrix_multiply (&Mo, &Mo, &M);
                //cairo_set_matrix (cr, &Mo);
            }

        protected:
            Object2D () 
                : myTransform (), myContext (0)
            { };

        private:
            Trafo2D<real> myTransform;
            cairo_t       *myContext;
    };
    
    /** 
     * @brief Line properties.
     */
    class LineTraits
    {
        public:
            LineTraits () 
                : myWidth (0.001), 
                  myColour (0.0, 0.0, 0.0, 1.0) 
            { }

            void apply (cairo_t* cr) const
            {
                cairo_set_source_rgba (cr, myColour.r, myColour.g, myColour.b, myColour.a);
                cairo_set_line_width (cr, myWidth);
            }

            real        width () const { return myWidth; };
            void        setWidth (real w) { myWidth = w; };
            const RGBA& colour () const { return myColour; };
            void        setColour (const RGBA& c) { myColour = c; };

        private:
            real myWidth;
            RGBA myColour;
    };

    class FontList
    {
        public:
            FontList ()
            {
                PangoFontMap* fm = pango_cairo_font_map_get_default ();
                int n_fam = 0;
                PangoFontFamily **families = 0;
                pango_font_map_list_families (fm, &families, &n_fam);

                printf ("n_fam = %d\n", n_fam);

                for (int i = 0; i < n_fam; ++i)
                {
                    printf ("%s\n", pango_font_family_get_name (families[i]));
                }

                g_free (families); // FIXME: Stimmt das so?
            }
    };

};
#endif

