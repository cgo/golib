#include <gogl/offfile.h>
#include <GL/gl.h>

#include <gofixedarray.h>
#include <govector.h>
#include <gofileio.h>
#include <golist.h>
#include <gopointcloud.h>

namespace goGL
{
    class OFFFilePrivate
    {
        public:
            OFFFilePrivate () 
                : vertices (0), 
                  faces (0),
                  cFlag (false),
                  nFlag (false),
                  fourFlag (false),
                  nVertices (0),
                  nFaces (0),
                  nEdges (0),
                  min (3),
                  max (3)
            {};
            ~OFFFilePrivate () {};

            goFixedArray<goVectorf>      vertices;
            goFixedArray<goVector<int> > faces;

            bool cFlag;
            bool nFlag;
            bool fourFlag;

            int nVertices;
            int nFaces;
            int nEdges;

            goVectorf min;
            goVectorf max;
    };
}

goGL::OFFFile::OFFFile ()
    : myPrivate (0)
{
    myPrivate = new goGL::OFFFilePrivate;
}

goGL::OFFFile::~OFFFile ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goGL::OFFFile::read (const char* filename)
{
    FILE* f = fopen (filename, "r");
    if (!f)
    {
        return false;
    }
    {
        goString line;
        goSize_t lineNumber = 0;
        if (!goFileIO::readASCIILine(f, line))
        {
            fclose (f);
            return false;
        }
        myPrivate->cFlag = false;
        myPrivate->nFlag = false;
        myPrivate->fourFlag = false;
        goIndex_t OFFindex = line.find ("OFF");
        if (OFFindex >= 0)
        {
            goIndex_t i = 0;
            while (line[i] != 'O' && i < line.getSize())
            {
                if (line[i] == 'C')
                    myPrivate->cFlag = true;
                if (line[i] == 'N')
                    myPrivate->nFlag = true;
                if (line[i] == '4')
                    myPrivate->fourFlag = true;
                ++i;
            }
            goFileIO::readASCIILine (f, line);
            ++lineNumber;
        }
        
        //= Else just assume this is an OFF file without the header.

        goList<goString> words;

        //= NVERTICES NFACES NEDGES
        line.getWords (words);
        if (words.getSize() != 3)
        {
            goString temp = "goGL::OFFFILE: Line ";
            temp += (int) lineNumber;
            temp += " should contain NVERTICES NFACES NEDGES, 3 numbers.";
            goLog::warning (temp.toCharPtr());
            return false;
        }
        myPrivate->nVertices = words.getFrontElement()->elem.toInt ();
        myPrivate->nFaces    = words.getFrontElement()->next->elem.toInt ();
        myPrivate->nEdges    = words.getFrontElement()->next->next->elem.toInt ();

        myPrivate->vertices.setSize (myPrivate->nVertices);
        myPrivate->faces.setSize (myPrivate->nFaces);

        //= Vertices
        int dimension = myPrivate->fourFlag ? 4 : 3;

        myPrivate->min.resize (dimension);
        myPrivate->max.resize (dimension);
        myPrivate->max.fill (-10000.f);
        myPrivate->min.fill (10000.f);

        int i = 0;
        while (!feof(f) && i < myPrivate->nVertices)
        {
            if (myPrivate->vertices[i].getSize() != (goSize_t)dimension)
            {
                myPrivate->vertices[i].resize (dimension);
            }
            goFileIO::readASCIILine (f, line);
            ++lineNumber;
            words.erase();
            line.getWords (words);
            if (words.getSize() != dimension)
            {
                goString temp = "goGL::OFFFile: incorrect dimension ";
                temp += (int) words.getSize();
                temp += " (expected ";
                temp += (int) dimension; 
                temp += ") in line ";
                temp += (int) lineNumber;
                goLog::warning (temp.toCharPtr());
                ++i;
                continue;
            }
            goList<goString>::Element* el = words.getFrontElement ();
            for (int j = 0; j < dimension && el; ++j, el = el->next)
            {
                myPrivate->vertices[i][j] = el->elem.toFloat ();
                if (myPrivate->vertices[i][j] < myPrivate->min[j])
                {
                    myPrivate->min[j] = myPrivate->vertices[i][j];
                }
                if (myPrivate->vertices[i][j] > myPrivate->max[j])
                {
                    myPrivate->max[j] = myPrivate->vertices[i][j];
                }
                //printf ("%f ", myPrivate->vertices[i][j]);
            }
            //printf ("\n");
            ++i;
        }
        if (i < myPrivate->nVertices)
        {
            goLog::warning ("goGL::OFFFile: Expected number of vertices could not be read.");
            fclose (f);
            return false;
        }
        //= Transform and copy
        {
        }

        //= 
        //= Faces
        //= 
        i = 0;
        while (!feof(f) && i < myPrivate->nFaces)
        {
            goFileIO::readASCIILine (f, line);
            ++lineNumber;
            words.erase();
            line.getWords (words);
            if (words.getSize() < 1)
            {
                goString temp = "goGL::OFFFile: incorrect line ";
                temp += (int) lineNumber;
                goLog::warning (temp.toCharPtr());
                ++i;
                continue;
            }
            int NV = words.getFrontElement()->elem.toInt ();
            if (words.getSize() < NV+1)
            {
                goString temp = "goGL::OFFFile: incorrect line ";
                temp += (int) lineNumber;
                goLog::warning (temp.toCharPtr());
                ++i;
                continue;
            }
            if (myPrivate->faces[i].getSize() != (goSize_t)NV)
            {
                myPrivate->faces[i].resize (NV);
            }
            goList<goString>::Element* el = words.getFrontElement()->next;
            for (int j = 0; j < NV && el; ++j, el = el->next)
            {
                myPrivate->faces[i][j] = el->elem.toInt ();
            }
            ++i;
        }
    }
    fclose (f);

    return true;
}

bool goGL::OFFFile::align ()
{
    goMatrixf M;
    goPointCloudf::getPrincipalAxes (myPrivate->vertices, M);
    goSize_t sz = myPrivate->vertices.getSize ();
    goVectorf mean;
    goPointCloudf::getCenterOfMass (myPrivate->vertices, mean);
    for (goSize_t i = 0; i < sz; ++i)
    {
        myPrivate->vertices[i] = M * (myPrivate->vertices[i] - mean);
    }
    return true;
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
    goSize_t sz = myPrivate->nFaces;
    while (i < sz)
    {
        // printf ("faces size == %d\n", myPrivate->faces[i].getSize());
        if (i < sz && myPrivate->faces[i].getSize() == 3)
        {
            glBegin (GL_TRIANGLES);
            while (i < sz && myPrivate->faces[i].getSize() == 3)
            {
                // printf ("3-face\n");
                // glColor3f (1.0, 1.0, 1.0);
                goVector<int>& face = myPrivate->faces[i];
                glVertex3fv (myPrivate->vertices[face[0]].getPtr());
                glVertex3fv (myPrivate->vertices[face[1]].getPtr());
                glVertex3fv (myPrivate->vertices[face[2]].getPtr());
                ++i;
            }
            glEnd ();
        }
        check_gl_error ("glend");
        if (i < sz && myPrivate->faces[i].getSize() >= 4)
        {
            glBegin (GL_QUADS);
            while (i < sz && myPrivate->faces[i].getSize() >= 4)
            {
                // printf ("4-face\n");
                // glColor3f (1.0, 1.0, 1.0);
                goVector<int>& face = myPrivate->faces[i];
                glVertex3fv (myPrivate->vertices[face[0]].getPtr());
                glVertex3fv (myPrivate->vertices[face[1]].getPtr());
                glVertex3fv (myPrivate->vertices[face[2]].getPtr());
                glVertex3fv (myPrivate->vertices[face[3]].getPtr());
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

const goVectorf& goGL::OFFFile::getMin () const
{
    return myPrivate->min;
}

const goVectorf& goGL::OFFFile::getMax () const
{
    return myPrivate->max;
}

goFixedArray<goVectorf>& goGL::OFFFile::getVertices ()
{
    return myPrivate->vertices;
}
