/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/glwidget.h>
// #include <gtk/gtkgl.h>
#include <goquaternion.h>

namespace goGUI
{
    class GLWidgetPrivate
    {
        public:
            GLWidgetPrivate () 
                : mode (IDLE), rotation (), rotationStart (2), rotationEnd (2)
            {
            };
            ~GLWidgetPrivate () {};

            enum Mode
            {
                IDLE,
                MOUSE_ROTATION
            };

            enum Mode mode;
            goQuaternion<goFloat> rotation;

            // goVectorf rotation;
            goVectorf rotationStart;
            goVectorf rotationEnd;
    };
}

goGUI::GLWidget::GLWidget ()
    : Gtk::GLArea (), myPrivate (0)
{
    myPrivate = new goGUI::GLWidgetPrivate;

    this->set_size_request (-1,-1);

    // GdkGLConfigMode mode = (GdkGLConfigMode)(GDK_GL_MODE_RGBA | GDK_GL_MODE_DOUBLE | GDK_GL_MODE_DEPTH);
    // GdkGLConfig* glconfig = gdk_gl_config_new_by_mode (mode);
//    gtk_widget_set_gl_capability (Gtk::Widget::gobj(),
//                                  glconfig,
//                                  NULL,
//                                  TRUE,
//                                  GDK_GL_RGBA_TYPE);

    this->add_events (Gdk::EXPOSURE_MASK | Gdk::POINTER_MOTION_MASK | Gdk::BUTTON_PRESS_MASK
            | Gdk::BUTTON_RELEASE_MASK);


    this->signal_render().connect (sigc::mem_fun(*this, &goGUI::GLWidget::renderSlot));
    // this->signal_expose_event().connect (sigc::mem_fun (*this, &goGUI::GLWidget::exposeSlot));
//    this->signal_motion_notify_event().connect (sigc::mem_fun (*this, &goGUI::GLWidget::motionSlot));
//    this->signal_button_press_event().connect (sigc::mem_fun (*this, &goGUI::GLWidget::buttonSlot));
//    this->signal_button_release_event().connect (sigc::mem_fun (*this, &goGUI::GLWidget::buttonSlot));
}

goGUI::GLWidget::~GLWidget ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::GLWidget::GLWidgetBegin ()
{
    // gdk_gl_drawable_gl_begin (gtk_widget_get_gl_drawable(Gtk::Widget::gobj()), gtk_widget_get_gl_context(Gtk::Widget::gobj()));
}

void goGUI::GLWidget::GLWidgetEnd ()
{
    // gdk_gl_drawable_gl_end (gtk_widget_get_gl_drawable(Gtk::Widget::gobj()));
}

/** 
 * @brief Can only be called between GLWidgetBegin and ...End.
 */
void goGUI::GLWidget::swapBuffers ()
{
	this->queue_draw();
//    GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (Gtk::Widget::gobj());
//    if (gdk_gl_drawable_is_double_buffered (gldrawable))
//        gdk_gl_drawable_swap_buffers (gldrawable);
}

#if 0
const goQuaternion<goFloat>& goGUI::GLWidget::getRotation () const
{
    return myPrivate->rotation;
}

bool goGUI::GLWidget::setRotation (const goQuaternion<goFloat>& r) 
{
    //if (r.getSize() == 4)
    {
        myPrivate->rotation = r;
        return true;
    }
    //return false;
}
#endif

/** 
 * @brief 2-Vector, mouse position last rotation start
 * 
 * @return 
 */
const goVectorf& goGUI::GLWidget::getRotationStart () const
{
    return myPrivate->rotationStart;
}

/** 
 * @brief 2-Vector, mouse position last rotation end
 * 
 * @return 
 */
const goVectorf& goGUI::GLWidget::getRotationEnd () const
{
    return myPrivate->rotationEnd;
}

bool goGUI::GLWidget::renderSlot (const Glib::RefPtr<Gdk::GLContext>& context)
{
	this->glDraw();
	return true;
}


void goGUI::GLWidget::glDraw ()
{
    // printf ("goGUI::GLWidget::glDraw(): doing nothing...\n");

    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    glFrustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
    glMatrixMode (GL_MODELVIEW);
    glViewport (0, 0, this->get_width(), this->get_height());

    glClear (GL_COLOR_BUFFER_BIT);
    glColor3f (1.0, 1.0, 1.0);
    glLoadIdentity ();
    glTranslatef (0.0, 0.0, -5.0);
    glScalef (1.0, 1.0, 1.0);

    glBegin (GL_TRIANGLES);
    {
        glVertex3f (-0.5, 0.0, 0.0);
        glVertex3f (0.5, 0.0, 0.0);
        glVertex3f (0.0, 0.5, 0.0);
    }
    glEnd ();
    glFlush ();
    this->swapBuffers ();
}

bool goGUI::GLWidget::exposeSlot (GdkEventExpose* e)
{
    this->GLWidgetBegin ();
    this->glDraw ();

    this->GLWidgetEnd ();
    return true;
}

bool goGUI::GLWidget::motionSlot (GdkEventMotion* e)
{
    if (!e)
        return false;

    if ((e->state & GDK_BUTTON1_MASK) && myPrivate->mode == GLWidgetPrivate::MOUSE_ROTATION)
    {
        myPrivate->rotationEnd[0] = e->x;
        myPrivate->rotationEnd[1] = e->y;

        this->GLWidgetBegin ();
        this->glDraw ();
        this->GLWidgetEnd ();

        myPrivate->rotationStart = myPrivate->rotationEnd;
    }

    return true;
}

bool goGUI::GLWidget::buttonSlot (GdkEventButton* e)
{
    if (!e)
        return false;

    switch (e->type)
    {
        case GDK_BUTTON_PRESS:
            // printf ("Button press: %f %f\n", e->x, e->y);
            myPrivate->mode = GLWidgetPrivate::MOUSE_ROTATION;
            myPrivate->rotationStart[0] = e->x;
            myPrivate->rotationStart[1] = e->y;
            break;
        case GDK_BUTTON_RELEASE:
            // printf ("Button release: %f %f\n", e->x, e->y);
            myPrivate->mode = GLWidgetPrivate::IDLE;
            break;
        default: return false; break;
    }

    return true;
}
