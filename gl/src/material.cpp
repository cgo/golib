/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/material.h>

goGL::Material::Material ()
    : goObjectBase (),
      myAmbient (4),
      mySpecular (4),
      myDiffuse (4),
      myShininess (0.0f),
      myEmission (4),
      myFace (GL_FRONT_AND_BACK)
{
    myAmbient.fill (0.2);
    myAmbient[3] = 1.0;
    mySpecular.fill (0.0);
    mySpecular[3] = 1.0;
    myDiffuse.fill (0.8);
    myDiffuse[3] = 1.0;
    myEmission.fill (0.0);
    myEmission[3] = 1.0;
}

goGL::Material::~Material ()
{
}

bool goGL::Material::operator() () const
{
    glMaterialfv (myFace, GL_AMBIENT, myAmbient.getPtr());
    glMaterialfv (myFace, GL_DIFFUSE, myDiffuse.getPtr());
    glMaterialfv (myFace, GL_SPECULAR, mySpecular.getPtr());
    glMaterialf (myFace, GL_SHININESS, myShininess);
    glMaterialfv (myFace, GL_EMISSION, myEmission.getPtr());

    return true;
}

bool goGL::Material::writeASCII (FILE* f) const
{
    bool ok = myAmbient.writeASCII (f);
    ok = ok && mySpecular.writeASCII (f);
    ok = ok && myDiffuse.writeASCII (f);
    goVectorf temp (1);
    temp[0] = myShininess;
    ok = ok && temp.writeASCII (f);
    ok = ok && myEmission.writeASCII (f);

    return ok;
}

bool goGL::Material::writeASCII (const char* filename) const
{
    FILE* f = ::fopen (filename, "w");
    if (!f) return false;
    bool ok = this->writeASCII (f);
    ::fclose (f);
    return ok;
}

bool goGL::Material::readASCII (FILE* f)
{
    bool ok = myAmbient.readASCII (f);
    ok = ok && mySpecular.readASCII (f);
    ok = ok && myDiffuse.readASCII (f);
    goVectorf temp (1);
    ok = ok && temp.readASCII (f);
    if (ok) myShininess = temp[0];
    ok = ok && myEmission.readASCII (f);

    return ok;
}

bool goGL::Material::readASCII (const char* filename)
{
    FILE* f = ::fopen (filename, "r");
    if (!f) return false;
    bool ok = this->readASCII (f);
    ::fclose (f);
    return ok;
}

