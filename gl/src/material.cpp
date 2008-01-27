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
    glMaterialfv (myFace, GL_EMSSION, myEmission.getPtr());

    return true;
}
