/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/offview.h>
#include <gogl/offfile.h>
#include <gogl/helper.h>
#include <gogl/camera.h>
#include <gogl/light.h>
#include <gogl/scene.h>

#include <gogui/glwidget.h>
#include <gogui/helper.h>

#include <gofileio.h>
#include <gopointcloud.h>
#include <gomath.h>

#include <stdio.h>
#include <stdlib.h>

#include <gogl/gl.h>

//#include <gtk/gtkgl.h>

static int check_gl_error (const char* name)
{
    int n = 0;
    if ((n = glGetError()) != GL_NO_ERROR)
    {
        printf ("%s error\n", name);
    }
    return n;
}

namespace goGUI
{
    class OFFViewPrivate
    {
        public:
            OFFViewPrivate () : spherical (3), 
                camera (100,100),
                light (GL_LIGHT0),
                scene (),
                //    position(3), up(3), focus(3), 
                so3(), 
                rotationMatrix (3,3),
                p0 (3),
                signal_changed(),
                signal_changed_final(),
                // signal_rotated(),
                caller_rotated(),
                rotationStart (2), rotationEnd (2)
            {
                spherical.fill (0.0f);
                spherical[2] = 1.0f;

                //position.fill (0.0f);
                //up[0] = 1.0f; up[1] = 0.0f; up[2] = 1.0f;
                //focus.fill (0.0f);

                rotationMatrix.setIdentity();
                goMath::sphereToEuclidean<goDouble> (0.0, 0.0, 1.0, &p0, 0);
            };
            ~OFFViewPrivate () {};

            goVectorf spherical;  //= Spherical coordinates

            //= Observer position
            //goVectorf position;
            //goVectorf up;
            //goVectorf focus;
            goGL::Camera camera;

            goGL::Light light;

            goGL::Scene scene;

            //= For rotation calculations.
            goMath::SO3<goDouble> so3;
            goMatrixd             rotationMatrix;
            goVectord             p0;  //= Origin, at (phi,theta) = (0,0).

            sigc::signal<void> signal_changed;       // emitted any time the image changed
            sigc::signal<void> signal_changed_final; // emitted e.g. when the mouse pointer is released after rotation
            // sigc::signal<void> signal_rotated;

            //goCaller0<int> caller_changed;
            //goCaller0<int> caller_changed_final;
            goCaller0<int> caller_rotated;

            enum Mode
            {
                IDLE,
                MOUSE_ROTATION,
                MOUSE_TILT
            };

            enum Mode mode;

            //= For mouse rotation
            goVectorf rotationStart; 
            goVectorf rotationEnd;
    };
}

goGUI::OFFView::OFFView () 
    : goGUI::GLWidget (),
      off (), myList (1), myPrivate (0)
{
    myPrivate = new OFFViewPrivate;
    //= Unused!

    this->add_events (Gdk::EXPOSURE_MASK | Gdk::POINTER_MOTION_MASK | Gdk::BUTTON_PRESS_MASK
            | Gdk::BUTTON_RELEASE_MASK);

    this->signal_motion_notify_event().connect (sigc::mem_fun (*this, &goGUI::OFFView::motionSlot));
    this->signal_button_press_event().connect (sigc::mem_fun (*this, &goGUI::OFFView::buttonSlot));
    this->signal_button_release_event().connect (sigc::mem_fun (*this, &goGUI::OFFView::buttonSlot));
}

goGUI::OFFView::~OFFView () 
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void goGUI::OFFView::load (const char* filename)
{
    this->off.read (filename);
    goFixedArray<goVectorf>& V = this->off.getVertices ();
    goSize_t sz = V.getSize ();
    goVectorf com;
    goPointCloud<goFloat>::getCenterOfMass (V, com);
    for (goSize_t i = 0; i < sz; ++i)
    {
        V[i] -= com;
    }
    this->off.align ();
    // this->myList = glGenLists (1);
    check_gl_error ("glGenLists");
    this->GLWidgetBegin ();
    // this->off.toList (this->myList);

    goGL::MeshObject* obj = new goGL::MeshObject;
    {
        //= Copy vertices and faces to obj
        goMatrixf& V = obj->getVertices();
        obj->getFaces() = this->off.getFaces();
        V.resize (this->off.getVertices().getSize(), 3);
        for (goSize_t i = 0; i < V.getRows(); ++i)
        {
            V.setRow (i, this->off.getVertices()[i]);
        }
        obj->init ();
        //= Add object to scene
        myPrivate->scene.add (goAutoPtr<goGL::DrawableObject>(obj));
        //= The scene is rendered (experimentally) later.
    }

    check_gl_error ("1");

    this->lighting ();

    check_gl_error ("2");
    this->GLWidgetEnd ();
}

void goGUI::OFFView::align ()
{
    this->off.align ();
    this->GLWidgetBegin ();
    this->off.toList (this->myList);
    this->GLWidgetEnd ();
}

void goGUI::OFFView::lighting ()
{
    glEnable (GL_NORMALIZE);  //= Automatically normalise normal vectors

    //= General ambient light
    GLfloat lm_ambient[] = { 0.4, 0.4, 0.4, 1.0 };
    glLightModeli (GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glLightModelfv (GL_LIGHT_MODEL_AMBIENT, lm_ambient);

    glShadeModel (GL_SMOOTH);
    myPrivate->light();
    glEnable (GL_LIGHTING);
    glEnable (GL_LIGHT0);
    glEnable (GL_DEPTH_TEST);
    return;

#if 0
    GLfloat mat_ambient[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat mat_shininess[] = { 50.0 };
    GLfloat light_position[] = { 1.0, 1.0, 1.0, 0.0 };

    // GLfloat light_position[] = { myPrivate->position[0], myPrivate->position[1], myPrivate->position[2], 1.0 };
    //GLfloat lm_ambient[] = { 0.4, 0.4, 0.4, 1.0 };
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
    check_gl_error ("lighting1");
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
    check_gl_error ("lighting2");
    glLightfv (GL_LIGHT0, GL_POSITION, light_position);

    glLightModeli (GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glLightModelfv (GL_LIGHT_MODEL_AMBIENT, lm_ambient);
    glEnable (GL_LIGHTING);
    glEnable (GL_LIGHT0);
    glEnable (GL_DEPTH_TEST);
    glShadeModel (GL_SMOOTH);
    return;

    glLightModelfv (GL_LIGHT_MODEL_AMBIENT, lm_ambient);
    check_gl_error ("lighting3");
    glLightModeli (GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    //check_gl_error ("lighting3");
    //glLightModeli (GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
    //check_gl_error ("lighting3");

    // const float pos[] = {this->off.getMax()[0] + 5.0f, this->off.getMax()[0] + 5.0, this->off.getMax()[0] + 5.0, 1.0f};
    // goVectorf center = (this->off.getMax() + this->off.getMin()) * 0.5f;
    // const float dir[] = {center[0] - pos[0], center[1] - pos[1], center[2] - pos[2]};
    const float pos1[] = {-2.0, 2.0, 1.0, 0.0};
    // glLightfv (GL_LIGHT1, GL_SPOT_DIRECTION, dir1);
    check_gl_error ("lighting4");
    glLightfv (GL_LIGHT1, GL_POSITION, pos1);
    check_gl_error ("lighting5");
    GLfloat light1_ambient[] = { 0.2, 0.2, 0.2, 1.0 };
    GLfloat light1_diffuse[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat light1_specular[] = { 1.0, 1.0, 1.0, 1.0 };
    glLightfv(GL_LIGHT1, GL_AMBIENT, light1_ambient);
    glLightfv(GL_LIGHT1, GL_DIFFUSE, light1_diffuse);
    glLightfv(GL_LIGHT1, GL_SPECULAR, light1_specular);
    glLightf(GL_LIGHT1, GL_CONSTANT_ATTENUATION, 1.5);
    glLightf(GL_LIGHT1, GL_LINEAR_ATTENUATION, 0.5);
    glLightf(GL_LIGHT1, GL_QUADRATIC_ATTENUATION, 0.2);

    // glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
    glEnable (GL_LIGHTING);
    check_gl_error ("lighting6");
    glEnable (GL_LIGHT0);
    check_gl_error ("lighting7");
    glEnable (GL_LIGHT1);
    check_gl_error ("lighting8");
    //glEnable (GL_LIGHT2);
    //glEnable (GL_LIGHT3);
    check_gl_error ("lighting8");
    glDepthFunc(GL_LEQUAL);
    check_gl_error ("lighting9");
    glEnable(GL_DEPTH_TEST);
    check_gl_error ("lighting10");
    // glEnable (GL_AUTO_NORMAL);
    check_gl_error ("lighting11");
    glShadeModel (GL_SMOOTH);
    //glShadeModel (GL_FLAT);
    check_gl_error ("lighting12");
#endif
}

void goGUI::OFFView::setLight (const goGL::Light& light)
{
    myPrivate->light = light;
    // (*myPrivate->scene.getLight(0)) = light;
    this->GLWidgetBegin ();
    this->lighting ();
    this->glDraw ();
    this->GLWidgetEnd ();
    this->swapBuffers ();
}

goGL::Light& goGUI::OFFView::getLight ()
{
    return myPrivate->light;
}

const goVectorf& goGUI::OFFView::getSphericalPosition () const
{
    return myPrivate->spherical;
}

void goGUI::OFFView::setRadius (goFloat r)
{
    myPrivate->spherical[2] = r;
    goMath::sphereToEuclidean (myPrivate->spherical[0], myPrivate->spherical[1], myPrivate->spherical[2], &myPrivate->camera.myPosition, &myPrivate->camera.myUp);
}

void goGUI::OFFView::setSphericalPosition (const goVectorf& r)
{
    myPrivate->spherical = r;

    goVectorf pos (3);
    goVectorf up (3);

    goMath::sphereToEuclidean<goFloat> (myPrivate->spherical[0], myPrivate->spherical[1],
            myPrivate->spherical[2], &pos, &up);
    goVectorf focus(3);
    focus.fill(0.0f);
    this->setView (pos, up, focus);
}

void goGUI::OFFView::setView (const goVectorf& position, const goVectorf& up, const goVectorf& focus)
{
    //myPrivate->position = position;
    //myPrivate->up = up;
    //myPrivate->focus = focus;

    myPrivate->camera.myPosition = position;
    myPrivate->camera.myUp = up;
    myPrivate->camera.myLookat = focus;

    // (*myPrivate->scene.getCamera()) = myPrivate->camera;
}

void goGUI::OFFView::glDraw ()
{
    glClearColor (0.0f, 0.0f, 0.0f, 0.0f);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    // glFrontFace (GL_CW);
    glDisable (GL_CULL_FACE);

    //= Experimentation with Scene
//    {
//        (*myPrivate->scene.getCamera()) = myPrivate->camera;
//        myPrivate->scene.getCamera()->myWidth  = this->get_width();
//        myPrivate->scene.getCamera()->myHeight = this->get_height();
//        
//        (*myPrivate->scene.getLight(0)) = myPrivate->light;
//        myPrivate->scene.render ();
//        glFlush ();
//        this->swapBuffers ();
//        myPrivate->signal_changed();
//        return;
//    }

    const goVectorf& min = this->off.getMin ();
    const goVectorf& max = this->off.getMax ();
    // printf ("max: %f %f %f\n", max[0], max[1], max[2]);
    // printf ("min: %f %f %f\n", min[0], min[1], min[2]);

    myPrivate->camera.myWidth = this->get_width();
    myPrivate->camera.myHeight = this->get_height();
    //myPrivate->camera();
    myPrivate->camera.setProjection();
    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity();
//    glLoadIdentity();
//    {
//        goFloat ttemp[] = {0.0f, 0.0f, 0.0f, 1.0f};
//        goVectorf temp(ttemp, 4, 1);
//        myPrivate->light.myPosition = temp;
//    }
    myPrivate->light.setPosition();

    //glPushMatrix();
    {
        myPrivate->camera.setViewingTransformation();

        const float list_diffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
        // glMaterialfv (GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, list_diffuse);
        glMaterialfv (GL_FRONT_AND_BACK, GL_AMBIENT, list_diffuse);
        // glMaterialfv (GL_FRONT_AND_BACK, GL_SPECULAR, list_diffuse);
        //glEnable (GL_LINE_SMOOTH);
        // glEnable (GL_POLYGON_SMOOTH);
        glCallList (this->myList);
    }
    //glPopMatrix();

    glFlush ();
    this->swapBuffers ();
    myPrivate->signal_changed();
    glDisable (GL_POLYGON_SMOOTH);
    glDisable (GL_LINE_SMOOTH);
}

goGL::OFFFile& goGUI::OFFView::getOFFFile ()
{
    return this->off;
}

sigc::signal<void>& goGUI::OFFView::signalChanged()
{
    return myPrivate->signal_changed;
}

sigc::signal<void>& goGUI::OFFView::signalChangedFinal()
{
    return myPrivate->signal_changed_final;
}

goCaller0<int>& goGUI::OFFView::callerRotated()
{
    return myPrivate->caller_rotated;
}

//sigc::signal<void>& goGUI::OFFView::signalRotated()
//{
//    return myPrivate->signal_rotated;
//}

bool goGUI::OFFView::motionSlot (GdkEventMotion* e)
{
    if (!e)
        return false;

    if ( (e->state & GDK_SHIFT_MASK) && (e->state & GDK_BUTTON1_MASK) && (myPrivate->mode == OFFViewPrivate::MOUSE_TILT) )
    {
        printf ("Tilting\n");
        myPrivate->rotationEnd[0] = e->x;
        myPrivate->rotationEnd[1] = e->y;

        goMatrixf R (3,3);
        goMath::SO3<goFloat> so3f;
        goVectorf axis = myPrivate->camera.myPosition.cross(myPrivate->camera.myUp).cross(myPrivate->camera.myUp);
        axis *= 1.0 / axis.norm2() * (myPrivate->rotationStart[0] - myPrivate->rotationEnd[0]) / 180.0 * M_PI;
        so3f.matrix (axis, R);
        myPrivate->camera.myUp *= R;

        //= Re-rectify (for numerical errors over time)
        myPrivate->camera.myUp = myPrivate->camera.myPosition.cross(myPrivate->camera.myUp).cross(myPrivate->camera.myPosition);
        myPrivate->camera.myUp *= 1.0 / myPrivate->camera.myUp.norm2();

        myPrivate->rotationStart = myPrivate->rotationEnd;

        // (*myPrivate->scene.getCamera()) = myPrivate->camera;
        this->queue_draw ();
        myPrivate->caller_rotated();
        return true;
    }

    if ((e->state & GDK_BUTTON1_MASK) && myPrivate->mode == OFFViewPrivate::MOUSE_ROTATION)
    {
        myPrivate->rotationEnd[0] = e->x;
        myPrivate->rotationEnd[1] = e->y;

        //= Calculate euclidean position of current view point
        goVectord p (3); goVectord up (3);
        p[0] = myPrivate->camera.myPosition[0];
        p[1] = myPrivate->camera.myPosition[1];
        p[2] = myPrivate->camera.myPosition[2];
        up[0] = myPrivate->camera.myUp[0];
        up[1] = myPrivate->camera.myUp[1];
        up[2] = myPrivate->camera.myUp[2];
        
        //goMath::sphereToEuclidean<goDouble> (myPrivate->spherical[0] / 180.0 * M_PI, myPrivate->spherical[1] / 180.0 * M_PI, myPrivate->spherical[2], &p, &up);
        //= Calculate rotation matrix
        goVectord x_axis = p.cross (up);
        x_axis *= 1.0 / x_axis.norm2();
        goVectord axis (3);
        axis = x_axis * (myPrivate->rotationEnd[1] - myPrivate->rotationStart[1]) -
            up * (myPrivate->rotationEnd[0] - myPrivate->rotationStart[0]);
        axis *= 1.0 / axis.norm2();
        axis *= (myPrivate->rotationStart - myPrivate->rotationEnd).norm2() / 180.0 * M_PI;
        goMatrixd R (3,3);
        myPrivate->so3.matrix (axis, R);
        //= Rotate and copy back to spherical coordinates so that we have a position on the view sphere.
        p *= R;
        up *= R;
        goDouble dummy = 0.0;
        goDouble phi, theta;
        goMath::euclideanToSphere<goDouble> (p, phi, theta, dummy);
        myPrivate->spherical[0] = phi;
        myPrivate->spherical[1] = theta;

        myPrivate->camera.myPosition[0] = p[0];
        myPrivate->camera.myPosition[1] = p[1];
        myPrivate->camera.myPosition[2] = p[2];
        myPrivate->camera.myUp[0] = up[0];
        myPrivate->camera.myUp[1] = up[1];
        myPrivate->camera.myUp[2] = up[2];

        //= Re-rectify (for numerical errors over time)
        myPrivate->camera.myUp = myPrivate->camera.myPosition.cross(myPrivate->camera.myUp).cross(myPrivate->camera.myPosition);
        myPrivate->camera.myUp *= 1.0 / myPrivate->camera.myUp.norm2();

        this->queue_draw ();

        //this->GLWidgetBegin ();
        //this->glDraw ();
        //this->GLWidgetEnd ();
        // (*myPrivate->scene.getCamera()) = myPrivate->camera;
        myPrivate->caller_rotated();

        myPrivate->rotationStart = myPrivate->rotationEnd;
    }

    return true;
}

bool goGUI::OFFView::buttonSlot (GdkEventButton* e)
{
    printf ("Event button with %p\n", e);
    if (!e)
        return false;

    switch (e->type)
    {
        case GDK_BUTTON_PRESS:
            printf ("Button press: %f %f\n", e->x, e->y);
            if (e->state & GDK_SHIFT_MASK)
            {
                myPrivate->mode = OFFViewPrivate::MOUSE_TILT;
            }
            else
            {
                myPrivate->mode = OFFViewPrivate::MOUSE_ROTATION;
            }
            myPrivate->rotationStart[0] = e->x;
            myPrivate->rotationStart[1] = e->y;
            break;
        case GDK_BUTTON_RELEASE:
            printf ("Button release: %f %f\n", e->x, e->y);
            myPrivate->mode = OFFViewPrivate::IDLE;
            myPrivate->signal_changed_final();
            break;
        default: return false; break;
    }

    return true;
}
