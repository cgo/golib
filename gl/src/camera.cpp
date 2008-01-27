#include <gogl/camera.h>
#include <GL/gl.h>
#include <GL/glu.h>

goGL::Camera::Camera (int width, int height, goFloat fov_angle, goFloat xy_aspect, goFloat near_clip, goFloat far_clip) 
    : goObjectBase (),
      myWidth(width), myHeight(height), myFOVAngle(fov_angle), myXYAspect(xy_aspect), myNearClip(near_clip), myFarClip(far_clip),
      myPosition(3), myLookat(3), myUp(3)
{
    myPosition[0] = -1.0f;
    myPosition[1] = 0.0f;
    myPosition[2] = 0.0f;
    myLookat.fill(0.0);
    myUp[0] = 0.0f;
    myUp[1] = 0.0f;
    myUp[2] = 1.0f;
}

goGL::Camera::~Camera ()
{
}

bool goGL::Camera::setProjection () const
{
    glViewport (0, 0, myWidth, myHeight);
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    float aspect = myXYAspect;
    if (aspect <= 0.0f)
    {
        aspect = (float)myWidth / (float)myHeight;
    }
    gluPerspective (30.0, aspect, 0.1, 1000.0);
    glMatrixMode (GL_MODELVIEW);

    return true;
}

bool goGL::Camera::setViewingTransformation () const
{
    const goVectorf& pos = myPosition;
    const goVectorf& up  = myUp;
    const goVectorf& focus  = myLookat;

    glLoadIdentity ();
    gluLookAt (pos[0], pos[1], pos[2],
               focus[0], focus[1], focus[2],
               up[0], up[1], up[2]);

    return true;
}

bool goGL::Camera::operator() () const
{
    bool ok = this->setProjection ();
    return ok && this->setViewingTransformation ();
}
