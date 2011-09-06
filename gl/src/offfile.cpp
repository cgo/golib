/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/offfile.h>
#include <GL/gl.h>

#include <gofixedarray.h>
#include <govector.h>
#include <gofileio.h>
#include <golist.h>
#include <gopointcloud.h>
#include <gomatrix.h>

namespace goGL
{
    class OFFFilePrivate
    {
        public:
            OFFFilePrivate () : normals(), vertexNormals() {};
            ~OFFFilePrivate () {};

            goMatrixf normals;
            goMatrixf vertexNormals;
    };
};

goGL::OFFFile::OFFFile ()
    : goOFFFile(), myPrivate (0)
{
    myPrivate = new OFFFilePrivate;
}

goGL::OFFFile::~OFFFile ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

static void check_gl_error (const char* name)
{
    if (glGetError() != GL_NO_ERROR)
    {
        printf ("%s error\n", name);
    }
}

bool goGL::OFFFile::read (const char* filename)
{
    if (!goOFFFile::read (filename))
        return false;

    this->calculateFaceNormals (myPrivate->normals);
    this->calculateNormals (myPrivate->vertexNormals);

    return true;
}

static inline void normal (const goMatrixf& normals, goSize_t i)
{
    const goVectorf n(0);
    normals.refRow (i, n);
    glNormal3f (n[0], n[1], n[2]);
}

bool goGL::OFFFile::draw ()
{
    goSize_t i = 0;
    goSize_t sz = this->getFaces().getSize(); 
    while (i < sz)
    {
        // printf ("faces size == %d\n", this->getFaces()[i].getSize());
        if (i < sz && this->getFaces()[i].getSize() == 3)
        {
            glBegin (GL_TRIANGLES);
            while (i < sz && this->getFaces()[i].getSize() == 3)
            {
                // printf ("3-face\n");
                // glColor3f (1.0, 1.0, 1.0);
                goVector<int>& face = this->getFaces()[i];
                //goVectorf n(3);
                //(this->getVertices()[face[2]] - this->getVertices()[face[0]]).cross (this->getVertices()[face[1]] - this->getVertices()[face[0]], n);
                //n *= 1.0f / n.norm2();
                //glNormal3f (myPrivate->normals(i,0), myPrivate->normals(i,1), myPrivate->normals(i,2));

                glNormal3f (myPrivate->vertexNormals(face[0],0), myPrivate->vertexNormals(face[0],1), myPrivate->vertexNormals(face[0],2));

                // normal (myPrivate->normals, face[0]);
                glVertex3fv (this->getVertices()[face[0]].getPtr());
                // normal (myPrivate->normals, face[1]);
                //glNormal3f (myPrivate->normals(i,0), myPrivate->normals(i,1), myPrivate->normals(i,2));
                glNormal3f (myPrivate->vertexNormals(face[1],0), myPrivate->vertexNormals(face[1],1), myPrivate->vertexNormals(face[1],2));
                glVertex3fv (this->getVertices()[face[1]].getPtr());
                // normal (myPrivate->normals, face[2]);
                //glNormal3f (myPrivate->normals(i,0), myPrivate->normals(i,1), myPrivate->normals(i,2));
                glNormal3f (myPrivate->vertexNormals(face[2],0), myPrivate->vertexNormals(face[2],1), myPrivate->vertexNormals(face[2],2));
                glVertex3fv (this->getVertices()[face[2]].getPtr());
                ++i;
            }
            glEnd ();
        }
        check_gl_error ("glend");
        if (i < sz && this->getFaces()[i].getSize() >= 4)
        {
            glBegin (GL_QUADS);
            while (i < sz && this->getFaces()[i].getSize() >= 4)
            {
                // printf ("4-face\n");
                // glColor3f (1.0, 1.0, 1.0);
                goVector<int>& face = this->getFaces()[i];
                //goVectorf n(3);
                //(this->getVertices()[face[2]] - this->getVertices()[face[0]]).cross (this->getVertices()[face[1]] - this->getVertices()[face[0]], n);
                //n *= 1.0f / n.norm2();
                //glNormal3f (n[0], n[1], n[2]);
                //glNormal3f (myPrivate->normals(i,0), myPrivate->normals(i,1), myPrivate->normals(i,2));
                glNormal3f (myPrivate->vertexNormals(face[0],0), myPrivate->vertexNormals(face[0],1), myPrivate->vertexNormals(face[0],2));
                //normal (myPrivate->normals, face[0]);
                glVertex3fv (this->getVertices()[face[0]].getPtr());
                //normal (myPrivate->normals, face[1]);
                //glNormal3f (myPrivate->normals(i,0), myPrivate->normals(i,1), myPrivate->normals(i,2));
                glNormal3f (myPrivate->vertexNormals(face[1],0), myPrivate->vertexNormals(face[1],1), myPrivate->vertexNormals(face[1],2));
                glVertex3fv (this->getVertices()[face[1]].getPtr());
                //normal (myPrivate->normals, face[2]);
                //glNormal3f (myPrivate->normals(i,0), myPrivate->normals(i,1), myPrivate->normals(i,2));
                glNormal3f (myPrivate->vertexNormals(face[2],0), myPrivate->vertexNormals(face[2],1), myPrivate->vertexNormals(face[2],2));
                glVertex3fv (this->getVertices()[face[2]].getPtr());
                //normal (myPrivate->normals, face[3]);
                //glNormal3f (myPrivate->normals(i,0), myPrivate->normals(i,1), myPrivate->normals(i,2));
                glNormal3f (myPrivate->vertexNormals(face[3],0), myPrivate->vertexNormals(face[3],1), myPrivate->vertexNormals(face[3],2));
                glVertex3fv (this->getVertices()[face[3]].getPtr());
                ++i;
            }
            glEnd ();
        }
    }
    check_gl_error ("glend");
    return true;
}

bool goGL::OFFFile::toList (int listName)
{
    glNewList (listName, GL_COMPILE);
    check_gl_error ("newlist");
    this->draw ();
    glEndList ();
    check_gl_error ("glendlist");

    return true;
}

