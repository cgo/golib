/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_IMAGEVIEW_H
#define GOGUI_IMAGEVIEW_H

#include <gogui/draw.h>
#include <gomatrix.h>
#include <gosignal3d.h>
#include <golist.h>
#include <goautoptr.h>
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif

#include <goplot/goplot.h>

namespace goGUI
{
    class ImageViewPrivate;

/** @addtogroup gui
 * @{
 */

    /** 
     * @brief Image display widget.
     *
     * @bug Check imageview.cpp for FIXME --- a setDimensions call to goPlot::Graph 
     * crashes somewhere in pango. The call is commented out for now.
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
            enum 
            {
                /// Value for changedCaller
                IMAGE_SET,
                /// Value for changedCaller
                IMAGE_REMOVED,
                /// Value for changedCaller
                CURRENT_IMAGE_CHANGED
            };
                
        public:
            ImageView ();
            virtual ~ImageView ();

            void draw ();

            void              setImage  (const goSignal3DBase<void>& image, goIndex_t index = -1);
            void              setImage  (int w, int h, int format = goPlot::Object2DImage::RGB24, goIndex_t index = -1);
            // goAutoPtr<goSignal3DBase<void> > getImage  ();
            goAutoPtr<goPlot::Object2DImage> getImageObject (goIndex_t index = -1); 
            goAutoPtr<goSignal3DBase<void> > getImage (goIndex_t index = -1);
            void                             removeImage (goIndex_t index = -1);
            void                             reorderImages (const std::vector<int>& order);
            goIndex_t         getImageCount () const;

            goAutoPtr<goPlot::Graph> graph ();

            void      setCurrentImage (goIndex_t i);
            goIndex_t currentImageIndex () const;

            goCaller1<void, int>& changedCaller ();
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
            virtual bool on_draw (Cairo::RefPtr<Cairo::Context> const& context) override;

            goAutoPtr<goSignal3DBase<void> > imageRef (goAutoPtr<goPlot::Object2DImage> img);

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
