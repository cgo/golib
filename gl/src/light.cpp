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
