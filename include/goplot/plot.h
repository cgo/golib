/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef PLOT_H
#define PLOT_H

#define NSPACE goPlot

#include <math.h>
#include <cairo.h>
// #include <pango/pangocairo.h>

#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif

namespace goPlot
{

    /** @addtogroup cairoplot
     * @{
     */
    typedef double real;

    //= Surface --> Graphs --> draw2d

    // template <class Points, class Real> class Graph;

    /**
     * @brief Base class for types which provide 2D points.
     * @param Real Real type (float or double)
     */
    template <class Real>
        class Points2DT
        {
            public:
                Points2DT () { };
                explicit Points2DT (int N) { };
                virtual ~Points2DT () { };

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

    typedef Points2DT<goDouble> Points2D;

    /**
     * @brief Simple 2D points provider. Just stores the points in a C array.
     */
    template <class Real>
        class Points2DSimple : public Points2DT<Real>
    {
        public:
            Points2DSimple (int count = 1)
                : Points2DT<Real> (), myPoints (0), mySize (0), myDelete (true)
            {
                myPoints = new Real[count * 2];
                mySize = count;
            };

            Points2DSimple (Real* ptr, size_t size)
                : Points2DT<Real> (), myPoints (ptr), mySize (size), myDelete (false)
            {
            };

            template <class T> Points2DSimple (const Points2DT<T>& other)
                : Points2DT<Real> (), myPoints (0), mySize (0), myDelete (true)
            {
                *this = other;
            };

            template <class T> Points2DSimple<Real>& operator= (const Points2DT<T>& other)
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
     * @brief Points2D class for accessing points in a goMath::Matrix.
     *
     * The matrix is stored in a goAutoPtr, so you can use a matrix object in some application code
     * and do changes to the same object used in the plotting code.
     *
     * An initial matrix object is created in any case, so there should always be a valid matrix
     * associated with a Points2DMatrix object.
     */
    template <class T>
        class Points2DMatrix : public Points2D
        {
            public:
                explicit Points2DMatrix (int N = 1) :
                    Points2D (),
                    M (new goMath::Matrix<T> (N,2)) { }

                /**
                 * @brief Constructor.
                 *
                 * @param m Matrix auto pointer. If the pointer is null, a new matrix is created
                 * instead with size 1.
                 */
                Points2DMatrix (goAutoPtr<goMath::Matrix<T> > m)
                    : Points2D (),
                      M (m)
                {
                    if (m.isNull())
                        M = new goMath::Matrix<T> (1,2);
                }

                /**
                 * @brief Constructor.
                 *
                 * Copies the points in m.
                 *
                 * @param m Configuration matrix, one point per row.
                 */
                Points2DMatrix (const goMath::Matrix<T>& m)
                    : Points2D (),
                      M (new goMatrix<T> (m))
                {
                }

                virtual ~Points2DMatrix () { }

                goMatrix<T>&       matrix () { return *M; }
                const goMatrix<T>& matrix () const { return *M; }

                //! Reimplement these.
                //! Return i'th point x value
                virtual goDouble x (int i) const { return (*M)(i, 0); }
                //! Return i'th point x value
                virtual goDouble y (int i) const { return (*M) (i, 1); }
                //! Set point number i
                virtual void set (int i, goDouble x, goDouble y) { (*M) (i, 0) = x; (*M) (i, 1) = y; }
                //! Return number of points
                virtual size_t size () const { return const_cast<Points2DMatrix<T>*>(this)->M->getRows (); } // sorry :)

            private:
                goAutoPtr<goMath::Matrix<T> > M;
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
            }

            double r, g, b, a;
    };

    /**
     * @brief Simple affine transformation representation (2x2 matrix and translation vector).
     *
     * Represents an affine transformation, essentially
     * in the same way Cairo does.
     *
     * The transformations consists of a 2x2 matrix M
       @verbatim
         xx xy
         yx yy
       @endverbatim
       and a translation vector <code> T = (x0,y0) </code>.
       The transformation applied to a point z then results in
       <code> M * z + T </code>.
     */
    template <class Real> class Trafo2DT
    {
        public:
            //= Matrix:
            //=   xx xy
            //=   yx yy
            //= Plus a translation (x0, y0)^T
            Trafo2DT (Real xx = 1.0, Real yx = 0.0, Real xy = 0.0, Real yy = 1.0, Real x0 = 0.0, Real y0 = 0.0);

            Trafo2DT (const Trafo2DT<Real>& t);
            Trafo2DT<Real>& operator= (const Trafo2DT<Real>& t);


            virtual ~Trafo2DT ();

            void print () const;

            void apply (cairo_t* cr);

            void operator*= (const Trafo2DT<Real>& M2);

            Real xx, xy, yx, yy, x0, y0;
    };

    typedef Trafo2DT<real> Trafo2D;

    /**
     * @brief Base class for all 2D drawable objects.
     */
    class Object2D
    {
        public:
            virtual ~Object2D ();

            virtual void draw () = 0;
            virtual void setContext (cairo_t* c);

            // Trafo2DT<real>& transform () { return myTransform; };
            const Trafo2D& transform () const;
            void setTransform (const Trafo2D& T);

            cairo_t* context ();
            const cairo_t* context () const;

            bool visible () const;
            void setVisible (bool t);

            void applyTransform (cairo_t* cr);

        protected:
            Object2D ();

        private:
            Trafo2DT<real> myTransform;
            cairo_t       *myContext;
            bool           myVisible;
    };

    /**
     * @brief Line properties.
     */
    class LineTraits
    {
        public:
            LineTraits ();

            void apply (cairo_t* cr) const;

            real        width () const;
            void        setWidth (real w);
            const RGBA& colour () const;
            void        setColour (const RGBA& c);

        private:
            real myWidth;
            RGBA myColour;
    };

    class FontList
    {
        public:
            FontList ();
    };
    /** @} */
};
#endif
