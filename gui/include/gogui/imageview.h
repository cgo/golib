#ifndef GOGUI_IMAGEVIEW_H
#define GOGUI_IMAGEVIEW_H

#include <gogui/draw.h>
#include <gomatrix.h>
#include <gosignal3d.h>
#include <golist.h>

namespace goGUI
{
    class ImageViewPrivate;

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
            goList<goMatrixd>& getCurves  ();

        protected:
            virtual bool on_expose_event (GdkEventExpose* event);

        private:
            ImageViewPrivate* myPrivate;
    };
};

#endif
