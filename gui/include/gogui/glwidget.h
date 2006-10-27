#ifndef GOGUI_GLWIDGET_H
#define GOGUI_GLWIDGET_H

#include <gtkmm.h>
#include <GL/gl.h>
#include <govector.h>
#include <goquaternion.h>

namespace goGUI
{
    class GLWidgetPrivate;

    class GLWidget : public Gtk::DrawingArea
    {
        public:
            GLWidget ();
            virtual ~GLWidget ();

            void GLWidgetBegin ();
            void GLWidgetEnd ();
            void swapBuffers ();

//            const goQuaternion<goFloat>& getRotation () const;
//            bool             setRotation (const goQuaternion<goFloat>& r);

            const goVectorf& getRotationStart () const;
            const goVectorf& getRotationEnd   () const;

            virtual void glDraw ();

            bool exposeSlot (GdkEventExpose* e);
            bool motionSlot (GdkEventMotion* e);
            bool buttonSlot (GdkEventButton* e);

        private:
            GLWidgetPrivate* myPrivate;
    };
}

#endif
