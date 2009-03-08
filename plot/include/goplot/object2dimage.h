#ifndef GOPLOT_OBJECT2DIMAGE_H
#define GOPLOT_OBJECT2DIMAGE_H

#include <goplot/plot.h>
#include <cairo/cairo.h>

#include <exception>

namespace NSPACE
{

    //= Cairo 2D drawing object
    //= Points must be derived from Points2D.
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


            Object2DImage ()
                : Object2D (), 
                  myCairoFormat (CAIRO_FORMAT_ARGB32),
                  mySurface (0)
            {
            }

            virtual ~Object2DImage () 
            { 
                if (mySurface)
                {
                    cairo_surface_destroy (mySurface);
                    mySurface = 0;
                }
            }

            /** 
             * @brief Set image.
             * 
             * @param image Image data (must match the format)
             * @param format Format, one of ARGB32, RGB24, A8, A1 (analog to Cairo)
             * @param width Width of the image
             * @param height Height of the image
             * @param stride Stride (number of bytes in each row)
             */
            void setImage (unsigned char* image, int format, int width, int height, int stride)
            {
                if (mySurface)
                {
                    cairo_surface_destroy (mySurface);
                    mySurface = 0;
                }

                switch (format)
                {
                    case ARGB32: myCairoFormat = CAIRO_FORMAT_ARGB32; break;
                    case RGB24: myCairoFormat = CAIRO_FORMAT_RGB24; break;
                    case A8: myCairoFormat = CAIRO_FORMAT_A8; break;
                    case A1: myCairoFormat = CAIRO_FORMAT_A1; break;
                    default: throw std::exception(); return;
                }

                mySurface = cairo_image_surface_create_for_data (image, myCairoFormat, width, height, stride);
            }

            /** 
             * @brief Creates an image of (width x height)
             * @note The allocated data has stride \c stride().
             * It is found with 
             * \c cairo_format_stride_for_width. Keep this in mind when working with the
             * data!
             *
             * @param format Format (see setImage)
             * @param width Width
             * @param height Height
             */
            void createImage (int format, int width, int height)
            {
                if (mySurface)
                {
                    cairo_surface_destroy (mySurface);
                    mySurface = 0;
                }

                switch (format)
                {
                    case ARGB32: myCairoFormat = CAIRO_FORMAT_ARGB32; break;
                    case RGB24: myCairoFormat = CAIRO_FORMAT_RGB24; break;
                    case A8: myCairoFormat = CAIRO_FORMAT_A8; break;
                    case A1: myCairoFormat = CAIRO_FORMAT_A1; break;
                    default: throw std::exception (); return;
                }

                int stride = cairo_format_stride_for_width (myCairoFormat, width);
                
                unsigned char* data = static_cast<unsigned char*> (::malloc (stride * height));

                mySurface = cairo_image_surface_create_for_data (data, myCairoFormat, width, height, stride);
            }

            /** 
             * @brief Get the data pointer.
             * 
             * @return Data pointer.
             */
            unsigned char* data ()
            {
                return cairo_image_surface_get_data (mySurface);
            }

            /** 
             * @brief Get image width.
             * 
             * @return Width in pixels
             */
            int width () const
            {
                return cairo_image_surface_get_width (mySurface);
            }

            /** 
             * @brief Get image height.
             * 
             * @return Height in pixels
             */
            int height () const
            {
                return cairo_image_surface_get_height (mySurface);
            }

            /** 
             * @brief Get the stride in bytes.
             * 
             * @return Stride (number of bytes per row).
             */
            int stride () const
            {
                return cairo_image_surface_get_stride (mySurface);
            }

            virtual void draw ()
            {
                cairo_t* cr = this->context ();
                if (!cr)
                    return;

                cairo_save (cr);


                this->applyTransform (cr);

                printf ("Drawing Object2DImage\n");

                cairo_set_source_surface (cr, mySurface, 0, 0);
                cairo_identity_matrix (cr);
                cairo_paint (cr);

                //= Set to identity matrix before stroke, so that line width is in device coordinates (e.g. pixels)
                // cairo_stroke (cr);

                cairo_restore (cr);
            }

        private:
             cairo_format_t myCairoFormat;
             cairo_surface_t *mySurface;
    };
};

#endif
