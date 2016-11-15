/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOPLOT_OBJECT2DIMAGE_H
#define GOPLOT_OBJECT2DIMAGE_H

#include <goplot/plot.h>
#include <cairo.h>

#include <exception>

//= cairo_format_t is an enum and there is no forward declaration for enums.
// #define cairo_format_t int

// struct cairo_surface_t;

namespace goPlot
{

    //= Cairo 2D drawing object
    //= Points must be derived from Points2D.

    /** @addtogroup cairoplot
     * @{
     */
    /**
     * @brief Image object.
     * @note Set the transform with \c setTransform() sensibly, so the dimensions fit in the
     * graph this object is added to.
     */
    class Object2DImage : public Object2D
    {
        public:

            //= Same as CAIRO_FORMAT_ARGB23 etc.
            enum {
                ARGB32 = 0,
                RGB24,
                A8,
                A1
            };


            Object2DImage ();

            virtual ~Object2DImage ();

            void setImage (unsigned char* image, int format, int width, int height, int stride);

            int format () const;

            void createImage (int format, int width, int height);

            unsigned char* data ();

            int width () const;

            int height () const;

            int stride () const;

            virtual void draw ();

        private:
             cairo_format_t myCairoFormat;
             cairo_surface_t *mySurface;
             goAutoPtr <unsigned char> myData; //= If data is allocated internally for an image, it is stored here.
    };

    /** @} */
};

// #undef cairo_format_t

#endif
