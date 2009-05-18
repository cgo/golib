#ifndef GOGUI_IMAGEVIEW_H
#define GOGUI_IMAGEVIEW_H

#include <gogui/draw.h>
#include <gomatrix.h>
#include <gosignal3d.h>
#include <golist.h>
#include <goautoptr.h>

#include <goplot.h>

namespace goGUI
{
    class ImageViewPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Image display widget.
     *
     * ImageView uses a goPlot::Graph to display objects with Cairo.
     * For an image set with \c setImage(), a buffer will be created, so the image data, if any,
     * will be copied. A wrapping goSignal3DBase referencing that buffer can be retrieved
     * with \c getImage(). The goPlot::Object2DImage that represents the same image data can be retrieved
     * with \c getImageObject().
     *
     * The data type of the image data is unsigned char, or GO_UINT8 in golib notation.
     * The number of channels is either 4 or 1, depending on whether the format is goPlot::Object2DImage::RGB24
     * , goPlot::Object2DImage::ARGB32 (both 4 channels) or goPlot::Object2DImage::A8.
     * This widget can display images from \c goSignal3DBase<void> objects.
     */
    class ImageView : public Gtk::DrawingArea
    {
        public:
            ImageView ();
            virtual ~ImageView ();

            void draw ();

            void              setImage  (const goSignal3DBase<void>& image);
            void              setImage  (int w, int h, int format = goPlot::Object2DImage::RGB24);
            // goAutoPtr<goSignal3DBase<void> > getImage  ();
            goAutoPtr<goPlot::Object2DImage> getImageObject (); 
            goAutoPtr<goSignal3DBase<void> > getImage ();
#if 0
            void              drawCurve (const goMatrixd& c);
            void              drawCurves ();
            void              drawImage ();
            void              addCurve  (const goMatrixd& M);
            void              removeCurve (goIndex_t i);
            void              setCurve (goIndex_t i, const goMatrixd& M);
            const goList<goMatrixd>& getCurves  () const;
#endif
        protected:
            virtual bool on_expose_event (GdkEventExpose* event);

        private:
            ImageView (const ImageView&);
            ImageView& operator= (const ImageView&);

        private:
            ImageViewPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
