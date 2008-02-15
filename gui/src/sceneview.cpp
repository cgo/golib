#include <gogui/sceneview.h>
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
    class SceneViewPrivate
    {
        public:
            SceneViewPrivate () : spherical (3), 
                scene (0),
                activeObject (-1),
                so3 (), 
                p0 (3),
                signal_changed(),
                signal_changed_final(),
                signal_rotated(),
                pointerStart (2), pointerEnd (2)
            {
                spherical.fill (0.0f);
                spherical[2] = 1.0f;

                //position.fill (0.0f);
                //up[0] = 1.0f; up[1] = 0.0f; up[2] = 1.0f;
                //focus.fill (0.0f);

                goMath::sphereToEuclidean<goDouble> (0.0, 0.0, 1.0, &p0, 0);
            };
            ~SceneViewPrivate () {};

            goVectorf spherical;  //= Spherical coordinates

            goAutoPtr<goGL::Scene> scene;
            goIndex_t activeObject;

            //= For rotation calculations.
            goMath::SO3<goDouble> so3;
            goVectord             p0;  //= Origin, at (phi,theta) = (0,0).

            sigc::signal<void> signal_changed;       // emitted any time the image changed
            sigc::signal<void> signal_changed_final; // emitted e.g. when the mouse pointer is released after rotation
            sigc::signal<void> signal_rotated;

            enum Mode
            {
                IDLE,
                MOUSE_ROTATION,
                MOUSE_TILT,
                MOUSE_TRANSLATION,
                MOUSE_TRANSLATION_Z,
                MOUSE_SCALE
            };

            enum Mode mode;

            //= For mouse rotation
            goVectorf pointerStart; 
            goVectorf pointerEnd;
    };
}

goGUI::SceneView::SceneView () 
    : goGUI::GLWidget (),
      myPrivate (0)
{
    myPrivate = new SceneViewPrivate;
    //= Unused!

    this->add_events (Gdk::EXPOSURE_MASK | Gdk::POINTER_MOTION_MASK | Gdk::BUTTON_PRESS_MASK
            | Gdk::BUTTON_RELEASE_MASK);

    this->signal_motion_notify_event().connect (sigc::mem_fun (*this, &goGUI::SceneView::motionSlot));
    this->signal_button_press_event().connect (sigc::mem_fun (*this, &goGUI::SceneView::buttonSlot));
    this->signal_button_release_event().connect (sigc::mem_fun (*this, &goGUI::SceneView::buttonSlot));
}

goGUI::SceneView::~SceneView () 
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

//= FIXME: This must go into the control.
/** 
 * @brief Load an OFF into the scene.
 *
 * @todo This is a little misplaced here --- move it to SceneControl.
 * 
 * @param filename 
 */
void goGUI::SceneView::loadOFF (const char* filename)
{
    if (myPrivate->scene.isNull())
    {
        return;
    }

    goOFFFile off;
    if (!off.read (filename))
    {
        goLog::warning ("goGUI::SceneView::loadOFF(): failed.");
        return;
    }
    goFixedArray<goVectorf>& V = off.getVertices ();
    goSize_t sz = V.getSize ();
    goVectorf com;
    goPointCloud<goFloat>::getCenterOfMass (V, com);
    for (goSize_t i = 0; i < sz; ++i)
    {
        V[i] -= com;
    }
    off.align ();

    goGL::MeshObject* obj = new goGL::MeshObject;
    {
        obj->setFilename (filename);
        //= Copy vertices and faces to obj
        goMatrixf& V = obj->getVertices();
        obj->getFaces() = off.getFaces();
        V.resize (off.getVertices().getSize(), 3);
        for (goSize_t i = 0; i < V.getRows(); ++i)
        {
            V.setRow (i, off.getVertices()[i]);
        }
        obj->init ();
        //= Add object to scene
        myPrivate->scene->add (goAutoPtr<goGL::DrawableObject>(obj));
        //= The scene is rendered (experimentally) later.
    }
}

/** 
 * @brief Set the light GL_LIGHT0.
 * 
 * @todo Remove --- the lights are all stored in the scene.
 *
 * @param light 
 */
void goGUI::SceneView::setLight (const goGL::Light& light)
{
    if (!myPrivate->scene.isNull())
    {
        (*myPrivate->scene->getLight(0)) = light;
        this->GLWidgetBegin ();
        myPrivate->scene->render ();
        this->swapBuffers ();
        this->GLWidgetEnd ();
    }
}

/** 
 * @brief Get goAutoPtr to the scene.
 * 
 * @return 
 */
goAutoPtr<goGL::Scene> goGUI::SceneView::getScene ()
{
    return myPrivate->scene;
}

/** 
 * @brief Set the scene.
 * 
 * @param s The scene. It is not copied, the same pointer is used.
 */
void goGUI::SceneView::setScene (goAutoPtr<goGL::Scene> s)
{
    myPrivate->scene = s;
}

/** 
 * @brief Get the spherical position of the camera around the scene.
 *
 * This is a remnant from \c OFFView -- keep it or not?
 * 
 * @return Vector containing the phi,theta pair.
 */
const goVectorf& goGUI::SceneView::getSphericalPosition () const
{
    return myPrivate->spherical;
}

/** 
 * @brief Set radius of the sphere around the origin at which the camera resides.
 *
 * This is a remnant from \c OFFView -- keep it or not?
 * 
 * @param r 
 */
void goGUI::SceneView::setRadius (goFloat r)
{
    if (myPrivate->scene.isNull())
        return;
    
    goGL::Camera& cam = *myPrivate->scene->getCamera();
    myPrivate->spherical[2] = r;
    goMath::sphereToEuclidean (myPrivate->spherical[0], myPrivate->spherical[1], myPrivate->spherical[2], &cam.myPosition, &cam.myUp);
}

/** 
 * @brief Set the spherical position of the camera.
 * 
 * This is a remnant from \c OFFView -- keep it or not?
 *
 * @param r 
 */
void goGUI::SceneView::setSphericalPosition (const goVectorf& r)
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

/** 
 * @brief Set the camera position.
 * 
 * This is a remnant from \c OFFView -- keep it or not?
 *
 * @param position Position of the camera
 * @param up       Up vector
 * @param focus    Point the camera looks at.
 */
void goGUI::SceneView::setView (const goVectorf& position, const goVectorf& up, const goVectorf& focus)
{
    if (myPrivate->scene.isNull())
        return;

    goGL::Camera& cam = *myPrivate->scene->getCamera();

    cam.myPosition = position;
    cam.myUp = up;
    cam.myLookat = focus;
}

/** 
 * @brief Sets the camera width/height to this widget's 
 * size and calls \c goGL::Scene::render().
 */
void goGUI::SceneView::glDraw ()
{
    if (myPrivate->scene.isNull())
        return;
    
    myPrivate->scene->getCamera()->myWidth = this->get_width();
    myPrivate->scene->getCamera()->myHeight = this->get_height();

    myPrivate->scene->render ();

    glFlush ();
    this->swapBuffers ();
    // myPrivate->signal_changed();   // FIXME this may cause problems.
}

sigc::signal<void> goGUI::SceneView::signalChanged()
{
    return myPrivate->signal_changed;
}

sigc::signal<void> goGUI::SceneView::signalChangedFinal()
{
    return myPrivate->signal_changed_final;
}

sigc::signal<void> goGUI::SceneView::signalRotated()
{
    return myPrivate->signal_rotated;
}

bool goGUI::SceneView::motionSlot (GdkEventMotion* e)
{
    if (myPrivate->scene.isNull() || !e)
        return false;

    goGL::Camera& cam = *myPrivate->scene->getCamera();
    goAutoPtr<goGL::DrawableObject> obj (0);

    if (myPrivate->activeObject >= 0)
    {
        obj = myPrivate->scene->getObject (myPrivate->activeObject);
    }

    goFloat xs = (float)this->get_width();
    goFloat ys = (float)this->get_height();
    goFloat maxs = goMath::max<goFloat> (xs, ys);

    if ( (e->state & GDK_SHIFT_MASK) && (e->state & GDK_BUTTON1_MASK) && (myPrivate->mode == SceneViewPrivate::MOUSE_TILT) )
    {
        printf ("Tilting\n");
        myPrivate->pointerEnd[0] = e->x;
        myPrivate->pointerEnd[1] = e->y;

        goMatrixf R (3,3);
        goMath::SO3<goFloat> so3f;
        goVectorf axis = cam.myPosition.cross(cam.myUp).cross (cam.myUp);
        axis *= 1.0 / axis.norm2() * (myPrivate->pointerStart[0] - myPrivate->pointerEnd[0]) / maxs * M_PI;
        so3f.matrix (axis, R);

        //= No object: re-position camera on view sphere
        if (obj.isNull())
        {
            cam.myUp *= R;

            //= Re-rectify (for numerical errors over time)
            cam.myUp = cam.myPosition.cross(cam.myUp).cross (cam.myPosition);
            cam.myUp *= 1.0 / cam.myUp.norm2();

            this->queue_draw ();
        }
        //= Otherwise to object rotation.
        //= If the object transformation is not done from within "glPushMatrix/glLoadIdentity/glPopMatrix" bounded code
        //= later, the axis needs to be calculated differently. In that case, identify the screen coordinate system
        //= with the object coordinate system.
        else
        {
            goVectorf rot = obj->getRotation ();
            goVectorf a (3); a[0] = rot[1]; a[1] = rot[2]; a[2] = rot[3];
            a *= 1.0f / a.norm2() * (rot[0] / 180.0f * M_PI);  //= Tangent element at Id
            // a += axis;
            goVectorf temp(0);
            goFloat _m[] = {1.0, 0.0, 0.0,
                0.0, 1.0, 0.0,
                0.0, 0.0, 1.0};
            goFloat _mn[] = {1.0, 0.0, 0.0,
                0.0, 1.0, 0.0,
                0.0, 0.0, 1.0};
            goMatrixf rot_mat (_m, 3, 3);
            so3f.matrix (a, rot_mat);
            goMatrixf rot_mat_new (_mn, 3, 3);
            so3f.exp (rot_mat, axis * -1.0f, rot_mat_new);
            so3f.vector (rot_mat_new, a);
            //goMatrixf rot_mat (_m, 3, 3);
            //so3f.matrix (a, rot_mat);
            //so3f.vector (rot_mat, a);
            goFloat angle = a.norm2();
            if (angle != 0.0f)
            {
                a /= angle;
            }
            else
            {
                a[0] = 1.0f;
                a[1] = 0.0f;
                a[2] = 0.0f;
            }
            obj->setRotation (angle / M_PI * 180.0f, a);
            this->queue_draw ();
        }

        myPrivate->pointerStart = myPrivate->pointerEnd;

        // (*myPrivate->scene.getCamera()) = myPrivate->camera;

        myPrivate->signal_rotated();
        myPrivate->signal_changed();
        return true;
    }

    if ((e->state & GDK_BUTTON1_MASK) && myPrivate->mode == SceneViewPrivate::MOUSE_ROTATION)
    {
        myPrivate->pointerEnd[0] = e->x;
        myPrivate->pointerEnd[1] = e->y;

        goVectorf p (3); goVectorf up (3);
        p[0] = cam.myPosition[0];
        p[1] = cam.myPosition[1];
        p[2] = cam.myPosition[2];
        up[0] = cam.myUp[0];
        up[1] = cam.myUp[1];
        up[2] = cam.myUp[2];

        //goMath::sphereToEuclidean<goDouble> (myPrivate->spherical[0] / 180.0 * M_PI, myPrivate->spherical[1] / 180.0 * M_PI, myPrivate->spherical[2], &p, &up);
        //= Calculate rotation matrix
        goVectorf x_axis = p.cross (up);
        x_axis *= 1.0 / x_axis.norm2();
        goVectorf axis (3);
        axis = x_axis * (myPrivate->pointerEnd[1] - myPrivate->pointerStart[1]) -
            up * (myPrivate->pointerEnd[0] - myPrivate->pointerStart[0]);
        axis *= 1.0 / axis.norm2();
        axis *= (myPrivate->pointerStart - myPrivate->pointerEnd).norm2() / maxs * M_PI;
        goMatrixf R (3,3);
        goMath::SO3<goFloat> so3f;
        so3f.matrix (axis, R);

        //= If no object is selected, re-position camera around view sphere.
        if (obj.isNull())
        {
            //= Calculate euclidean position of current view point
            //= Rotate and copy back to spherical coordinates so that we have a position on the view sphere.
            p *= R;
            up *= R;
            goFloat dummy = 0.0;
            goFloat phi, theta;
            goMath::euclideanToSphere<goFloat> (p, phi, theta, dummy);
            myPrivate->spherical[0] = phi;
            myPrivate->spherical[1] = theta;

            cam.myPosition[0] = p[0];
            cam.myPosition[1] = p[1];
            cam.myPosition[2] = p[2];
            cam.myUp[0] = up[0];
            cam.myUp[1] = up[1];
            cam.myUp[2] = up[2];

            //= Re-rectify (for numerical errors over time)
            cam.myUp = cam.myPosition.cross(cam.myUp).cross (cam.myPosition);
            cam.myUp *= 1.0 / cam.myUp.norm2 ();

            this->queue_draw ();
        }
        else
        {
            goVectorf rot = obj->getRotation ();
            goVectorf a (3); a[0] = rot[1]; a[1] = rot[2]; a[2] = rot[3];
            a *= 1.0f / a.norm2() * (rot[0] / 180.0f * M_PI);  //= Tangent element at Id
            goVectorf temp(0);
            goFloat _m[] = {1.0, 0.0, 0.0,
                0.0, 1.0, 0.0,
                0.0, 0.0, 1.0};
            goFloat _mn[] = {1.0, 0.0, 0.0,
                0.0, 1.0, 0.0,
                0.0, 0.0, 1.0};
            goMatrixf rot_mat (_m, 3, 3);
            so3f.matrix (a, rot_mat);
            goMatrixf rot_mat_new (_mn, 3, 3);
            so3f.exp (rot_mat, axis * -1.0f, rot_mat_new);
            so3f.vector (rot_mat_new, a);
            printf ("a:\n");
            a.print ();
            goFloat angle = a.norm2();
            if (angle != 0.0f)
            {
                a /= angle;
            }
            else
            {
                a[0] = 1.0f;
                a[1] = 0.0f;
                a[2] = 0.0f;
            }
            obj->setRotation (angle / M_PI * 180.0f, a);
            this->queue_draw ();
        }
        myPrivate->signal_rotated();
        myPrivate->signal_changed();
        myPrivate->pointerStart = myPrivate->pointerEnd;
    }

    if (myPrivate->mode == SceneViewPrivate::MOUSE_TRANSLATION)
    {
        myPrivate->pointerEnd[0] = e->x;
        myPrivate->pointerEnd[1] = e->y;

        goFloat _st[] = {myPrivate->pointerEnd[0] - myPrivate->pointerStart[0], -1.0 * (myPrivate->pointerEnd[1] - myPrivate->pointerStart[1]), 0.0f};
        goVectorf screen_trans (_st, 3, 1);
        
        if (!obj.isNull())
        {
            goFloat w = cam.viewPortWidth (obj->getTranslation());
            goFloat h = cam.viewPortHeight (obj->getTranslation());
            goFloat delta_x = -screen_trans[0] / (float)cam.myWidth * w;
            goFloat delta_y = -screen_trans[1] / (float)cam.myHeight * h;

            screen_trans[0] = 0.5 * delta_x;
            screen_trans[1] = 0.5 * delta_y;

            //goVectorf n = cam.myLookat - cam.myPosition;
            //n /= n.norm2 ();
            //goFloat l2 = n * (obj->getTranslation() - cam.myPosition);
            
            //screen_trans[0] = delta_x * l2 / cam.myNearClip;
            //screen_trans[1] = delta_y * l2 / cam.myNearClip;
        }

        // screen_trans *= 0.01;
        goVectorf x = cam.myPosition.cross(cam.myUp);
        x *= 1.0 / x.norm2();
        goVectorf y = cam.myUp;
        goVectorf z = cam.myPosition * -1.0f;

        //= Only translate objects
        if (!obj.isNull())
        {
            obj->setTranslation (obj->getTranslation() + (x * screen_trans[0] + y * screen_trans[1] + z * screen_trans[2]));
            this->queue_draw ();
        }

        myPrivate->pointerStart = myPrivate->pointerEnd;
        myPrivate->signal_changed();
    }

    if (myPrivate->mode == SceneViewPrivate::MOUSE_TRANSLATION_Z)
    {
        myPrivate->pointerEnd[0] = e->x;
        myPrivate->pointerEnd[1] = e->y;

        goFloat _st[] = {0.0f, 0.0f, -1.0f * (myPrivate->pointerEnd[1] - myPrivate->pointerStart[1])};
        goVectorf screen_trans (_st, 3, 1);

        if (!obj.isNull())
        {
            // goFloat w = cam.viewPortWidth ();
            goFloat h = cam.viewPortHeight (obj->getTranslation());
            // goFloat delta_x = screen_trans[0] / (float)cam.myWidth * w;
            goFloat delta_y = -screen_trans[2] / (float)cam.myHeight * h;

            screen_trans[2] = delta_y; 

            //goVectorf n = cam.myLookat - cam.myPosition;
            //n /= n.norm2 ();
            //goFloat l2 = n * (obj->getTranslation() - cam.myPosition);
            
            //// screen_trans[0] = delta_x * l2 / cam.myNearClip;
            //screen_trans[2] = delta_y * l2 / cam.myNearClip;
        }

        // screen_trans *= 0.01;
        goVectorf x = cam.myPosition.cross(cam.myUp) * -1.0f;
        x *= 1.0 / x.norm2();
        goVectorf y = cam.myUp;
        goVectorf z = cam.myPosition * -1.0f;

        //= Only translate objects
        if (!obj.isNull())
        {
            obj->setTranslation (obj->getTranslation() + (x * screen_trans[0] + y * screen_trans[1] + z * screen_trans[2]));
            this->queue_draw ();
        }

        myPrivate->pointerStart = myPrivate->pointerEnd;
        myPrivate->signal_changed();
    }

    if (myPrivate->mode == SceneViewPrivate::MOUSE_SCALE)
    {
        myPrivate->pointerEnd[0] = e->x;
        myPrivate->pointerEnd[1] = e->y;

        if (!obj.isNull())
        {
            goFloat scaleAdd = (myPrivate->pointerEnd[1] - myPrivate->pointerStart[1]) / maxs;
            goVectorf scale = obj->getScale ();
            scale += scaleAdd;
            obj->setScale (scale);
            this->queue_draw ();
        }

        myPrivate->pointerStart = myPrivate->pointerEnd;
        myPrivate->signal_changed();
    }

    return true;
}

bool goGUI::SceneView::buttonSlot (GdkEventButton* e)
{
    printf ("Event button with %p\n", e);
    if (!e)
        return false;

    switch (e->type)
    {
        case GDK_BUTTON_PRESS:
            printf ("Button press: %f %f\n", e->x, e->y);
            switch (e->button)
            {
                case 1:
                    if (e->state & GDK_SHIFT_MASK)
                    {
                        myPrivate->mode = SceneViewPrivate::MOUSE_TILT;
                    }
                    else
                    {
                        myPrivate->mode = SceneViewPrivate::MOUSE_ROTATION;
                    }
                    break;
                case 2:
                    if (e->state & GDK_SHIFT_MASK)
                    {
                        myPrivate->mode = SceneViewPrivate::MOUSE_TRANSLATION_Z;
                    }
                    else
                    {
                        myPrivate->mode = SceneViewPrivate::MOUSE_TRANSLATION;
                    }
                    break;
                case 3:
                    myPrivate->mode = SceneViewPrivate::MOUSE_SCALE;
                    break;
                default: break;
            }
            myPrivate->pointerStart[0] = e->x;
            myPrivate->pointerStart[1] = e->y;
            break;
        case GDK_BUTTON_RELEASE:
            printf ("Button release: %f %f\n", e->x, e->y);
            myPrivate->mode = SceneViewPrivate::IDLE;
            myPrivate->signal_changed_final();
            break;
        default: return false; break;
    }

    return true;
}

void goGUI::SceneView::setActiveObject (goIndex_t i)
{
    myPrivate->activeObject = i;
}
