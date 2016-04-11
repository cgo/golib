/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/drawableobject.h>
#include <gogl/gl.h>

namespace goGL
{
    class DrawableObjectPrivate
    {
        public:
            DrawableObjectPrivate () 
                : lighting (true),
                  shadeModel (GL_SMOOTH),
                  material () {};
            ~DrawableObjectPrivate () {};

            bool           lighting;
            GLenum         shadeModel;  // GL_SMOOTH or GL_FLAT
            goGL::Material material;
    };
};

goGL::DrawableObject::DrawableObject ()
    : Object (),
      myPrivate (0)
{
    myPrivate = new DrawableObjectPrivate;
}

goGL::DrawableObject::~DrawableObject ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goGL::DrawableObject::DrawableObject (const DrawableObject& o)
    : Object (o),
      myPrivate (0)
{
    myPrivate = new DrawableObjectPrivate;
    *this = o;
}

goGL::DrawableObject& goGL::DrawableObject::operator= (const DrawableObject& o)
{
    *(Object*)this = *(Object*)(&o);
    this->setLighting (o.getLighting());
    this->setShadeModel (o.getShadeModel());
    this->setMaterial (o.getMaterial());

    return *this;
}

//bool goGL::DrawableObject::operator () () const
//{
//}
//
static void check_gl_error (const char* name)
{
    if (glGetError() != GL_NO_ERROR)
    {
        printf ("%s error\n", name);
    }
}
bool goGL::DrawableObject::toList (int listName)
{
    glNewList (listName, GL_COMPILE);
    check_gl_error ("newlist");
    bool ok = this->draw ();
    glEndList ();
    check_gl_error ("glendlist");

    return ok;
}

void goGL::DrawableObject::setLighting (bool l)
{
    myPrivate->lighting = l;
}

void goGL::DrawableObject::setShadeModel (GLenum e)
{
    myPrivate->shadeModel = e;
}

void goGL::DrawableObject::setMaterial (const goGL::Material& mat)
{
    myPrivate->material = mat;
}

goGL::Material& goGL::DrawableObject::getMaterial ()
{
    return myPrivate->material;
}

const goGL::Material& goGL::DrawableObject::getMaterial () const
{
    return myPrivate->material;
}

/** 
 * @brief Sets lighting and shade model.
 *
 * Call before drawing in the beginning of Scene::render().
 * 
 * @return True.
 */
bool goGL::DrawableObject::setRenderParameters () const
{
    if (myPrivate->lighting)
    {
        glEnable (GL_LIGHTING);
    }
    else
    {
        glDisable (GL_LIGHTING);
    }

    glShadeModel (myPrivate->shadeModel);

    return true;
}

bool goGL::DrawableObject::writeASCII (FILE* f) const
{
    bool ok = Object::writeASCII (f);
    if (!ok) return false;
    return myPrivate->material.writeASCII (f);
}

bool goGL::DrawableObject::readASCII (FILE* f)
{
    bool ok = Object::readASCII (f);
    if (!ok) return false;
    return myPrivate->material.readASCII (f);
}

bool goGL::DrawableObject::getLighting () const
{
    return myPrivate->lighting;
}

GLenum goGL::DrawableObject::getShadeModel () const
{
    return myPrivate->shadeModel;
}
