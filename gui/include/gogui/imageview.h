#ifndef GOGUI_IMAGEVIEW_H
#define GOGUI_IMAGEVIEW_H

#include <gogui/draw.h>
#include <gomatrix.h>
#include <gosignal3d.h>
#include <golist.h>

namespace goGUI
{
    class ImageViewPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Image display widget.
     *
     * This widget can display images from \c goSignal3DBase<void> objects
     * and additionally can draw curves given as point matrices.
     */
    class ImageView : public Gtk::DrawingArea
    {
        public:
            ImageView ();
            virtual ~ImageView ();

            void              drawCurve (const goMatrixd& c);
            void              drawCurves ();
            void              drawImage ();
            void              setImage  (const goSignal3DBase<void>& image);
            goSignal3D<void>& getImage  ();
            void              addCurve  (const goMatrixd& M);
            void              removeCurve (goIndex_t i);
            void              setCurve (goIndex_t i, const goMatrixd& M);
            const goList<goMatrixd>& getCurves  () const;

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
