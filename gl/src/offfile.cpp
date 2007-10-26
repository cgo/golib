#include <gogl/offfile.h>
#include <GL/gl.h>

#include <gofixedarray.h>
#include <govector.h>
#include <gofileio.h>
#include <golist.h>
#include <gopointcloud.h>

goGL::OFFFile::OFFFile ()
    : goOFFFile()
{
}

goGL::OFFFile::~OFFFile ()
{
}

static void check_gl_error (const char* name)
{
    if (glGetError() != GL_NO_ERROR)
    {
        printf ("%s error\n", name);
    }
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
                glVertex3fv (this->getVertices()[face[0]].getPtr());
                glVertex3fv (this->getVertices()[face[1]].getPtr());
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
                glVertex3fv (this->getVertices()[face[0]].getPtr());
                glVertex3fv (this->getVertices()[face[1]].getPtr());
                glVertex3fv (this->getVertices()[face[2]].getPtr());
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

