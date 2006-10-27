#include <gogui/offview.h>
#include <gogl/offfile.h>
#include <gogui/glwidget.h>
#include <gogui/helper.h>

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
            OFFViewPrivate () : rotationAngles (3)
            {
                rotationAngles.fill (0.0f);
            };
            ~OFFViewPrivate () {};

            goVectorf rotationAngles;
    };
}

goGUI::OFFView::OFFView () 
    : goGUI::GLWidget (),
      off (), myList (1), myRotation (), myPrivate (0)
{
    myPrivate = new OFFViewPrivate;
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
    // this->myList = glGenLists (1);
    check_gl_error ("glGenLists");
    this->GLWidgetBegin ();
    this->off.toList (this->myList);
    glLoadIdentity ();
    this->lighting ();
    this->GLWidgetEnd ();
}

void goGUI::OFFView::lighting ()
{
    GLfloat mat_ambient[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat mat_shininess[] = { 50.0 };
    GLfloat light_position[] = { 0.0, 0.0, 0.0, 1.0 };
    GLfloat lm_ambient[] = { 0.4, 0.4, 0.4, 1.0 };
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat_ambient);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular);
    check_gl_error ("lighting1");
    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, mat_shininess);
    check_gl_error ("lighting2");
    glLightfv (GL_LIGHT0, GL_POSITION, light_position);
    glLightModelfv (GL_LIGHT_MODEL_AMBIENT, lm_ambient);
    glLightModeli (GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
    check_gl_error ("lighting3");

    const float pos[] = {this->off.getMax()[0] + 5.0f, this->off.getMax()[0] + 5.0, this->off.getMax()[0] + 5.0, 1.0f};
    goVectorf center = (this->off.getMax() + this->off.getMin()) * 0.5f;
    const float dir[] = {center[0] - pos[0], center[1] - pos[1], center[2] - pos[2]};
    glLightfv (GL_LIGHT1, GL_SPOT_DIRECTION, dir);
    check_gl_error ("lighting4");
    glLightfv (GL_LIGHT1, GL_POSITION, pos);
    check_gl_error ("lighting5");

    // glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
    glEnable (GL_LIGHTING);
    check_gl_error ("lighting6");
    glEnable (GL_LIGHT0);
    check_gl_error ("lighting7");
    glEnable (GL_LIGHT1);
    check_gl_error ("lighting8");
    glDepthFunc(GL_LEQUAL);
    check_gl_error ("lighting9");
    glEnable(GL_DEPTH_TEST);
    check_gl_error ("lighting10");
    glEnable (GL_AUTO_NORMAL);
    check_gl_error ("lighting11");
    // glShadeModel (GL_SMOOTH);
    glShadeModel (GL_FLAT);
    check_gl_error ("lighting12");
}

const goVectorf& goGUI::OFFView::getRotation () const
{
    return myPrivate->rotationAngles;
}

void goGUI::OFFView::setRotation (const goVectorf& r)
{
    myPrivate->rotationAngles = r;
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
    gluPerspective (60.0, 1.0, 0.1, 1000.0);
    glMatrixMode (GL_MODELVIEW);

    glClearColor (0.0f, 0.0f, 0.0f, 0.0f);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity ();
    check_gl_error ("1");

    goVectorf eye (3);
    goVectorf center (3);
    goVectorf up (3);
    eye = max * 1.5f;
    //            eye[0] = 2.0;
    //            eye[1] = 2.0;
    //            eye[2] = 2.0;
    center = (max + min) * 0.5;
    goVectorf temp = eye - center;
    temp *= 1.0f / temp.norm2();
    const float f[3] = {1.0f, 0.0f, 0.0f};
    up[0] = temp[1] * f[2] - temp[2] * f[1];
    up[1] = temp[2] * f[0] - temp[0] * f[2];
    up[2] = temp[0] * f[1] - temp[1] * f[0];
    if (up.norm2() < 1e-3)
    {
        const float f[3] = {0.0f, 1.0f, 0.0f};
        up[0] = temp[1] * f[2] - temp[2] * f[1];
        up[1] = temp[2] * f[0] - temp[0] * f[2];
        up[2] = temp[0] * f[1] - temp[1] * f[0];
    }
    up *= 1.0 / up.norm2();

    gluLookAt(eye[0], eye[1], eye[2],
            center[0], center[1], center[2],
            up[0], up[1], up[2]);

    printf ("Up: %f %f %f\n", up[0], up[1], up[2]);
    printf ("center: %f %f %f\n", center[0], center[1], center[2]);
    printf ("eye: %f %f %f\n", eye[0], eye[1], eye[2]);

    //= Rotation via mouse -- does not work right.
#if 0
    goVectorf rotMovement = this->getRotationEnd() - this->getRotationStart();
    goFloat rotNorm = rotMovement.norm2 ();
    if (fabs(rotNorm) > 1e-2)
        //= Calculate rotation in object coordinates.
    {
        rotMovement *= 1.0f / rotNorm;

        go3Vector<goFloat> axis;
        axis.x = rotMovement[1];
        axis.y = rotMovement[0]; //-temp[0]; // y-Koordinate negiert, weil GL genau die
        // entgegengerichtete y-Achse benutzt.
        axis.z = 0.0f;
        {
            //= Find camera coordinate system
            go3Vector<goFloat> e2_ (up[0], up[1], up[2]);
            go3Vector<goFloat> e3_ (temp[0], temp[1], temp[2]);
            e3_ *= 1.0f / e3_.abs();
            go3Vector<goFloat> e1_ = e2_;
            e1_.cross (e3_);
            e1_ *= 1.0f / e1_.abs();
            go3Vector<goFloat> e1 (e1_.x, e2_.x, e3_.x);
            go3Vector<goFloat> e2 (e1_.y, e2_.y, e3_.y);
            go3Vector<goFloat> e3 (e1_.z, e2_.z, e3_.z);

            axis = go3Vector<goFloat> (axis * e1, axis * e2, axis * e3);
        }
        goQuaternion<goFloat> rot;
        rot.setRotation (rotNorm / static_cast<float>(this->get_width() + this->get_height()) * M_PI, axis);
        myRotation = myRotation * rot;

        printf ("rotation: %f %f %f %f\n", myRotation.v.x,  myRotation.v.y, myRotation.v.z, myRotation.scalar);
    }
    glTranslatef (center[0], center[1], center[2]);
    // glRotatef (rot.getRotationAngle() * 360.0f, rot.v.x, rot.v.y, rot.v.z);
    //= Rotation around axis in camera coordinates
    float rotMatrix [16];
    myRotation.getRotationMatrix (rotMatrix);
    glMultMatrixf (rotMatrix);
    // glRotatef (myRotation.getRotationAngle() * 360.0f, corr_axis.x, corr_axis.y, corr_axis.z);
    glTranslatef (-center[0], -center[1], -center[2]);
#endif

    glColor3f (1.0, 1.0, 1.0);
    //            glLoadIdentity ();
    //            goVectorf scale (3);
    //            scale[0] = 1.0f / (max[0] - min[0]);
    //            scale[1] = 1.0f / (max[1] - min[1]);
    //            scale[2] = 1.0f / (max[2] - min[2]);
    //            glScalef (1.0, 1.0, 1.0);
    //            glTranslatef (0.0, 0.0, -1.5 - max[2]);
    // glTranslatef (-(max[0] + min[0]) * 0.5, -(max[1] + min[1]) * 0.5, -1.5 - max[2]);
    // glScalef (scale[0], scale[1], scale[2]);
    check_gl_error ("2");

    glTranslatef (center[0], center[1], center[2]);
    glRotatef (myPrivate->rotationAngles[0], 1.0f, 0.0f, 0.0f);
    glRotatef (myPrivate->rotationAngles[1], 0.0f, 1.0f, 0.0f);
    glRotatef (myPrivate->rotationAngles[2], 0.0f, 0.0f, 1.0f);
    glTranslatef (-center[0], -center[1], -center[2]);

    //glPushMatrix ();
    //glLoadIdentity ();
    //this->lighting ();
    //glPopMatrix ();

    const float list_diffuse[] = {0.3f, 0.3f, 0.7f, 1.0f};
    glMaterialfv (GL_FRONT_AND_BACK, GL_DIFFUSE, list_diffuse);
    glCallList (this->myList);
    // this->off.draw ();
    // int n = check_gl_error ("calllist");
    // printf ("%s\n", gluErrorString (n));
    glFlush ();
    this->swapBuffers ();
}
