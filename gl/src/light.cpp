/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/light.h>
#include <GL/gl.h>
#include <godefs.h>

goGL::Light::Light (GLenum light_enum)
    : goObjectBase(),
      myPosition(4),
      myAmbient(4),
      mySpecular(4),
      myDiffuse(4),
      myLightEnum(light_enum),
      myEnabled(false)
{
    this->setClassID (GO_GL_LIGHT);
    myPosition[0] = 1.0f;
    myPosition[1] = 1.0f;
    myPosition[2] = 1.0f;
    myPosition[3] = 0.0f;
    myAmbient.fill(0.4f);
    myAmbient[3] = 1.0f;
    mySpecular.fill(1.0f);
    myDiffuse.fill(1.0f);
    if (myLightEnum == GL_LIGHT0)
    {
        myEnabled = true;
    }
}

goGL::Light::~Light ()
{
}

bool goGL::Light::setPosition () const
{
    glLightfv (myLightEnum, GL_POSITION, myPosition.getPtr());
    return true;
}

bool goGL::Light::operator() () const
{
    this->setPosition ();

    // glLightModeli (GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glLightfv (myLightEnum, GL_AMBIENT, myAmbient.getPtr());
    glLightfv (myLightEnum, GL_SPECULAR, mySpecular.getPtr());
    glLightfv (myLightEnum, GL_DIFFUSE, myDiffuse.getPtr());
    // glLightModelfv (GL_LIGHT_MODEL_AMBIENT, myAmbient.getPtr());
    // glLightModelfv (GL_LIGHT_MODEL_AMBIENT, myAmbient.getPtr());

    return true;
}

bool goGL::Light::writeASCII (FILE* f) const
{
    bool ok = myPosition.writeASCII (f);
    ok = ok && myAmbient.writeASCII (f);
    ok = ok && mySpecular.writeASCII (f);
    ok = ok && myDiffuse.writeASCII (f);
    goVectorf temp(1);
    temp[0] = (float)myLightEnum;
    ok = ok && temp.writeASCII (f);
    temp[0] = myEnabled ? 1.0f : 0.0f;
    ok = ok && temp.writeASCII (f);

    return ok;
}

bool goGL::Light::writeASCII (const char* filename) const
{
    FILE* f = ::fopen (filename, "w");
    if (!f) return false;
    bool ok = this->writeASCII (f);
    ::fclose (f);
    return ok;
}

bool goGL::Light::readASCII (FILE* f)
{
    bool ok = myPosition.readASCII (f);
    ok = ok && myAmbient.readASCII (f);
    ok = ok && mySpecular.readASCII (f);
    ok = ok && myDiffuse.readASCII (f);
    goVectorf temp(1);
    ok = ok && temp.readASCII (f);
    if (ok) myLightEnum = (GLenum)temp[0];
    ok = ok && temp.readASCII (f);
    if (ok) myEnabled = (temp[0] == 1.0f) ? true : false;

    return ok;
}

bool goGL::Light::readASCII (const char* filename)
{
    FILE* f = ::fopen (filename, "r");
    if (!f) return false;
    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}
