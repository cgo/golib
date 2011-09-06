/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/camera.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <godefs.h>

goGL::Camera::Camera (int width, int height, goFloat fov_angle, goFloat xy_aspect, goFloat near_clip, goFloat far_clip) 
    : goObjectBase (),
      myWidth(width), myHeight(height), myFOVAngle(fov_angle), myXYAspect(xy_aspect), myNearClip(near_clip), myFarClip(far_clip),
      myPosition(3), myLookat(3), myUp(3)
{
    this->setClassID (GO_GL_CAMERA);
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
    gluPerspective (myFOVAngle, aspect, myNearClip, myFarClip);
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

/** 
 * @brief Calculate the viewport width in scene coordinates.
 * 
 * @return Width.
 */
goFloat goGL::Camera::viewPortWidth (const goVectorf& at) const
{
    return this->viewPortHeight(at) * myXYAspect;
}

/** 
 * @brief Calculate the viewport height in scene coordinates.
 * 
 * @return Height.
 */
goFloat goGL::Camera::viewPortHeight (const goVectorf& at) const
{
    goFloat l = (at - this->myPosition) * ( (this->myLookat - this->myPosition) / ((this->myLookat - this->myPosition).norm2 ()) );
    return 2.0f * l * ::tan (myFOVAngle * 0.5f);
}

bool goGL::Camera::writeASCII (FILE* f) const
{
    goVectorf temp (6);
    temp[0] = myWidth;
    temp[1] = myHeight;
    temp[2] = myFOVAngle;
    temp[3] = myXYAspect;
    temp[4] = myNearClip;
    temp[5] = myFarClip;
    
    bool ok = temp.writeASCII (f);
    ok = ok && myPosition.writeASCII (f);
    ok = ok && myLookat.writeASCII (f);
    ok = ok && myUp.writeASCII (f);

    return ok;

}

bool goGL::Camera::writeASCII (const char* filename) const
{
    FILE* f = ::fopen (filename, "w");
    if (!f) return false;
    bool ok = this->writeASCII (f);
    ::fclose (f);
    return ok;
}

bool goGL::Camera::readASCII (FILE* f)
{
    goVectorf temp (6);
    bool ok = temp.readASCII (f);
    if (ok && temp.getSize() == 6)
    {
        myWidth = (int)temp[0];
        myHeight = (int)temp[1];
        myFOVAngle = temp[2];
        myXYAspect = temp[3];
        myNearClip = temp[4];
        myFarClip = temp[5];
    }
    ok = ok && myPosition.readASCII (f);
    ok = ok && myLookat.readASCII (f);
    ok = ok && myUp.readASCII (f);

    return ok;
}

bool goGL::Camera::readASCII (const char* filename)
{
    FILE* f = ::fopen (filename, "r");
    if (!f) return false;
    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}
