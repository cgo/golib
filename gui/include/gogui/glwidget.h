/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_GLWIDGET_H
#define GOGUI_GLWIDGET_H

#include <gtkmm.h>
#include <gogl/gl.h>
#include <govector.h>
#include <goquaternion.h>

namespace goGUI
{
    class GLWidgetPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief OpenGL widget.
     *
     * Derives from Gtk::DrawingArea. The GL capabilities are added using the
     * gdkgl extension library.
     * When using GL commands, always use them between calls to
     * \c GLWidgetBegin and \c GLWidgetEnd.
     * Use \c swapBuffers to swap buffers.
     *
     * When deriving from \c GLWidget, implement \c glDraw() to contain 
     * any drawing routines (such as drawing opengl scenes).
     */
    class GLWidget : public Gtk::GLArea
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

            bool renderSlot (const Glib::RefPtr<Gdk::GLContext>& context);

            bool exposeSlot (GdkEventExpose* e);
            bool motionSlot (GdkEventMotion* e);
            bool buttonSlot (GdkEventButton* e);

        private:
            GLWidgetPrivate* myPrivate;
    };
/** 
 * @}
 */
}

#endif
