#include <gogui/offview.h>
#include <gogl/offfile.h>
#include <gogl/helper.h>
#include <gogui/glwidget.h>
#include <gogui/helper.h>

#include <gofileio.h>
#include <gopointcloud.h>

#include <stdio.h>
#include <stdlib.h>

#include <GL/glu.h>

#include <gtk/gtkgl.h>

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
            OFFViewPrivate () : rotation (3), position(3), up(3), focus(3)
            {
                rotation.fill (0.0f);
                rotation[0] = 1.0f;

                position.fill (0.0f);
                up[0] = 1.0f; up[1] = 0.0f; up[2] = 1.0f;
                focus.fill (0.0f);
            };
            ~OFFViewPrivate () {};

            goVectorf rotation;

            goVectorf position;
            goVectorf up;
            goVectorf focus;
    };
}

goGUI::OFFView::OFFView () 
    : goGUI::GLWidget (),
      off (), myList (1), myRotation (), myPrivate (0)
{
    myPrivate = new OFFViewPrivate;
    //= Unused.
    myRotation.setRotation (0.0f, go3Vector<goFloat> (1.0f, 0.0f, 0.0f));
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
    // this->off.align ();
    // this->myList = glGenLists (1);
    check_gl_error ("glGenLists");
    this->GLWidgetBegin ();
    this->off.toList (this->myList);
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

    GLfloat mat_ambient[] = { 1.0, 1.0, 1.0, 0.0 };
    GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 0.0 };
    GLfloat mat_shininess[] = { 50.0 };
    GLfloat light_position[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat lm_ambient[] = { 0.4, 0.4, 0.4, 0.0 };
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
    check_gl_error ("lighting1");
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
    check_gl_error ("lighting2");
    glLightfv (GL_LIGHT0, GL_POSITION, light_position);
    glLightModelfv (GL_LIGHT_MODEL_AMBIENT, lm_ambient);
    check_gl_error ("lighting3");
    //glLightModeli (GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
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
    glEnable (GL_AUTO_NORMAL);
    check_gl_error ("lighting11");
    glShadeModel (GL_SMOOTH);
    //glShadeModel (GL_FLAT);
    check_gl_error ("lighting12");
}

const goVectorf& goGUI::OFFView::getRotation () const
{
    return myPrivate->rotation;
}

void goGUI::OFFView::setRotation (const goVectorf& r)
{
    // this line is obsolete, as is myPrivate->rotation.
    myPrivate->rotation = r;

    goVectorf pos (3);
    goVectorf up (3);

    goDouble temp = myPrivate->rotation[1] / 180.0f * M_PI;

    goMath::sphereToEuclidean<goFloat> (myPrivate->rotation[0] / 180.0f * M_PI, temp, 
            myPrivate->rotation[2], &pos, &up);
    goVectorf focus(3);
    focus.fill(0.0f);
    this->setView (pos, up, focus);
}

void goGUI::OFFView::setView (const goVectorf& position, const goVectorf& up, const goVectorf& focus)
{
    myPrivate->position = position;
    myPrivate->up = up;
    myPrivate->focus = focus;
}

void goGUI::OFFView::glDraw ()
{
    // glFrontFace (GL_CW);
    glDisable (GL_CULL_FACE);
    
    const goVectorf& min = this->off.getMin ();
    const goVectorf& max = this->off.getMax ();
    printf ("max: %f %f %f\n", max[0], max[1], max[2]);
    printf ("min: %f %f %f\n", min[0], min[1], min[2]);

    glViewport (0, 0, this->get_width(), this->get_height());
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    gluPerspective (30.0, (float)this->get_width() / (float)this->get_height(), 0.1, 1000.0);
    glMatrixMode (GL_MODELVIEW);

    glClearColor (0.0f, 0.0f, 0.0f, 0.0f);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity ();
    check_gl_error ("1");

    this->lighting ();

    check_gl_error ("2");

//    if (myPrivate->rotation[1] == 0.0f)
//    {
//        myPrivate->rotation[1] = 0.0001f; //= bloody libc cos() ...
//    }
//    else if (myPrivate->rotation[1] == 180.0f)
//    {
//        myPrivate->rotation[1] = 180.0f - 0.0001f; //= bloody libc cos() ...
//    }
    // goDouble temp = myPrivate->rotation[1] / 180.0f * M_PI;
    const goVectorf& pos = myPrivate->position;
    const goVectorf& up  = myPrivate->up;
    const goVectorf& focus  = myPrivate->focus;

    glLoadIdentity ();
    gluLookAt (pos[0], pos[1], pos[2],
               focus[0], focus[1], focus[2],
               up[0], up[1], up[2]);

    //glPushMatrix ();
    //glLoadIdentity ();
    //GLfloat light_position[] = { pos[0] + up[0], pos[1] + up[1], pos[2] + up[2], 0.0 };
    //GLfloat light_dir[] = { -pos[0] - up[0], -pos[1] - up[1], -pos[2] - up[2], 1.0 };
    //glLightfv (GL_LIGHT0, GL_POSITION, light_position);
    //glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 75.0);
    //glLightfv (GL_LIGHT0, GL_SPOT_DIRECTION, light_dir);
    //glEnable (GL_LIGHT0);
    //glPopMatrix ();

    const float list_diffuse[] = {0.3f, 0.3f, 0.7f, 1.0f};
    glMaterialfv (GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, list_diffuse);
    glEnable (GL_LINE_SMOOTH);
    glEnable (GL_POLYGON_SMOOTH);
    glCallList (this->myList);
    // this->off.draw ();
    // int n = check_gl_error ("calllist");
    // printf ("%s\n", gluErrorString (n));
    glFlush ();
    this->swapBuffers ();
    glDisable (GL_POLYGON_SMOOTH);
    glDisable (GL_LINE_SMOOTH);
}

goGL::OFFFile& goGUI::OFFView::getOFFFile ()
{
    return this->off;
}
