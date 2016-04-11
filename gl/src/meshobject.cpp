/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogl/meshobject.h>
#include <gogl/gl.h>
#include <govector.h>
#include <gofixedarray.h>
#include <gomatrix.h>
#include <gofileio.h>
#include <goofffile.h>
#include <gopointcloud.h>

namespace goGL
{
    class MeshObjectPrivate
    {
        public:
            MeshObjectPrivate ()
                : vertices(0,0),
                  faces(0),
                  normals(0,0),
                  faceNormals(0,0),
                  listnumber(1),
                  filename("")
            { };
            ~MeshObjectPrivate () { };

            goMatrixf                    vertices;
            goFixedArray<goVector<int> > faces;

            goMatrixf                    normals;
            goMatrixf                    faceNormals;

            GLuint                       listnumber;

            goString                     filename;
    };
};

goGL::MeshObject::MeshObject ()
    : DrawableObject (),
      myPrivate (0)
{
    myPrivate = new MeshObjectPrivate;
}

goGL::MeshObject::~MeshObject ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goGL::MeshObject::MeshObject (const MeshObject& o)
    : DrawableObject (),
      myPrivate (0)
{
    myPrivate = new MeshObjectPrivate;
}

goGL::MeshObject& goGL::MeshObject::operator= (const MeshObject& o)
{
    myPrivate->vertices = o.getVertices ();
    myPrivate->faces = o.getFaces ();
    this->setFilename (o.getFilename());
    this->init ();

    return *this;
}

/** 
 * @brief Calls everything necessary for OpenGL to draw this object.
 * 
 * @return True if successful, false otherwise.
 */
bool goGL::MeshObject::operator () () const
{
    glPushMatrix ();
    {
        if (!this->setRenderParameters())
            return false;
        
        this->getMaterial() ();

        const goVectorf& rotation = this->getRotation();
        const goVectorf& t = this->getTranslation();
        const goVectorf& s = this->getScale();
        glTranslatef (t[0], t[1], t[2]);
        glScalef (s[0], s[1], s[2]);
        glRotatef (rotation[0], rotation[1], rotation[2], rotation[3]);
        glCallList (myPrivate->listnumber);
    }
    glPopMatrix ();

    return true;
}

/** 
 * @brief Searches for faces containing double vertices and and removes
 * the bad faces.
 * 
 * @return True if successful, false otherwise.
 */
bool goGL::MeshObject::removeDoubles ()
{
    goFixedArray<goVector<int> >& faces = myPrivate->faces;
    goList<goVector<int> > newFaces;

    goSize_t sz = faces.getSize();
    for (goSize_t i = 0; i < sz; ++i)
    {
        goVector<int>& v = faces[i];
        goSize_t n = v.getSize ();
        bool bad_face = false;
        for (goSize_t j = 0; j < n - 1 && !bad_face; ++j)
        {
            int vj = v[j];
            for (goSize_t k = j + 1; k < n; ++k)
            {
                if (vj == v[k])
                {
                    bad_face = true;
                    break;
                }
            }
        }
        if (!bad_face)
            newFaces.append (v);
    }
    faces.setSize (newFaces.getSize());
    goList<goVector<int> >::Element* el = newFaces.getFrontElement ();
    sz = faces.getSize ();
    goSize_t i = 0;
    while (el && i < sz)
    {
        faces[i] = el->elem;
        el = el->next;
        ++i;
    }

    return true;
}

/** 
 * @brief Get the minimum coordinates of all vertices.
 * 
 * @return goVectorf pointer, containing the minimum coordinates.
 */
goAutoPtr<goVectorf> goGL::MeshObject::getMin () const
{
    goSize_t N = myPrivate->vertices.getColumns ();
    goAutoPtr<goVectorf> ret = goAutoPtr<goVectorf> (new goVectorf(N));
    goVectorf col (0);
    for (goSize_t i = 0; i < N; ++i)
    {
        myPrivate->vertices.refColumn (i, col);
        (*ret)[i] = goMath::min (col);
    }
    return ret;
}

/** 
 * @brief Get the maximum coordinates of all vertices.
 * 
 * @return goVectorf pointer, containing the maximum coordinates.
 */
goAutoPtr<goVectorf> goGL::MeshObject::getMax () const
{
    goSize_t N = myPrivate->vertices.getColumns ();
    goAutoPtr<goVectorf> ret = goAutoPtr<goVectorf> (new goVectorf(N));
    goVectorf col (0);
    for (goSize_t i = 0; i < N; ++i)
    {
        myPrivate->vertices.refColumn (i, col);
        (*ret)[i] = goMath::max (col);
    }
    return ret;
}

/** 
 * @brief Get the matrix with the vertices of this object.
 * 
 * @return Matrix containing a vertex per row.
 */
goMatrixf& goGL::MeshObject::getVertices ()
{
    return myPrivate->vertices;
}

/** 
 * @brief Get the faces of this object.
 * 
 * @return Array containing the indices of vertices for each face.
 */
goFixedArray<goVector<int> >& goGL::MeshObject::getFaces ()
{
    return myPrivate->faces;
}

/** 
 * @brief See non-const version.
 */
const goMatrixf& goGL::MeshObject::getVertices () const
{
    return myPrivate->vertices;
}

/** 
 * @brief See non-const version.
 */
const goFixedArray<goVector<int> >& goGL::MeshObject::getFaces () const
{
    return myPrivate->faces;
}

/** 
 * @brief Build adjacency lists for all vertices of this object.
 *
 */
void goGL::MeshObject::getAdjacencyLists (goFixedArray<goList<int> >& ret) const
{
    const goFixedArray<goVector<int> >& faces = this->getFaces();
    int N = this->getVertices().getRows();
    ret.setSize (0);
    ret.setSize (N);
    int Nfaces = faces.getSize();
    for (int fi = 0; fi < Nfaces; ++fi)
    {
        const goVector<int>& v = faces[fi];
        int Nv = v.getSize();
        for (int j = 0; j < Nv; ++j)
        {
            int neigh1 = (j + 1) % Nv;
            int neigh2 = (j - 1);
            if (neigh2 < 0)
                neigh2 += Nv;
            neigh1 = v[neigh1];
            neigh2 = v[neigh2];
            if (!ret[v[j]].contains(neigh1)) 
            {
                ret[v[j]].append(neigh1);
            }
            if (!ret[v[j]].contains(neigh2)) 
            {
                ret[v[j]].append(neigh2);
            }
        }
    }
}

/** 
 * @brief For each vertex, compute the list of faces it is contained in.
 * 
 * @param ret For each vertex, contains the list of faces the vertex is contained in.
 */
void goGL::MeshObject::getAdjacentFaces (goFixedArray<goList<int> >& ret) const
{
    const goFixedArray<goVector<int> >& faces = this->getFaces();
    int N = this->getVertices().getRows();
    ret.setSize (0);
    ret.setSize (N);
    int Nfaces = faces.getSize();
    for (int fi = 0; fi < Nfaces; ++fi)
    {
        const goVector<int>& v = faces[fi];
        goSize_t Nv = v.getSize();
        for (goSize_t i = 0; i < Nv; ++i)
        {
            if (!ret[v[i]].contains (fi))
            {
                ret[v[i]].append (fi);
            }
        }
    }
}

template <class T>
static bool contains (const goFixedArray<T>& a, T v)
{
    goSize_t sz = a.getSize();
    for (goSize_t i = 0; i < sz; ++i)
    {
        if (a[i] == v)
            return true;
    }
    return false;
}
template <class T>
static bool contains (const goVector<T>& a, T v)
{
    goSize_t sz = a.getSize();
    for (goSize_t i = 0; i < sz; ++i)
    {
        if (a[i] == v)
            return true;
    }
    return false;
}

/** 
 * @brief Calculate normals for all vertices and faces.
 * 
 * @param normals Normals for each vertex.
 * @param face_normals Normals for each face.
 */
void goGL::MeshObject::calculateNormals (goMatrixf& normals, goMatrixf& face_normals) const
{
    const goMatrixf& vertices = this->getVertices();

    const goSize_t N = vertices.getRows ();
    if (N <= 0)
        return;
    const goSize_t dim = vertices.getColumns();

    this->calculateFaceNormals (face_normals);
    goFloat _normal[3];
    goVectorf normal (_normal, 3, 1);

    goFixedArray<goList<int> > adj;
    this->getAdjacencyLists (adj);

    goFixedArray<goList<int> > adjFaces;
    this->getAdjacentFaces (adjFaces);

    normals.resize (N, dim);

    if (adj.getSize() != N || adjFaces.getSize() != N)
    {
        goLog::error ("goGL::MeshObject::calculateNormals(): adjacency size != vertex count");
        return;
    }

    normals.resize (N, dim);
    normals.fill (0.0f);
    goVectorf fn_row (0);
    for (goSize_t i = 0; i < N; ++i)
    {
        normals.refRow (i, normal);
        normal.fill (0.0f);
        goList<int>::Element* el = adjFaces[i].getFrontElement();
        while (el)
        {
            face_normals.refRow (el->elem, fn_row);
            normal += fn_row; // / fn_row.norm2();
            el = el->next;
        }

        if (normal.norm2() > 0.0f)
            normal *= 1.0f / normal.norm2();
        else
        {
            printf ("goGL::MeshObject: ***************** NORMAL WAS 0 !!!! *********************\n");
            normal.fill (1.0/3.0);
            printf ("Face normals:\n");
            goList<int>::Element* el = adjFaces[i].getFrontElement();
            while (el)
            {
                goVectorf temp (0);
                printf ("face %d: ", el->elem);
                face_normals.refRow (el->elem, temp);
                for (goSize_t j = 0; j < temp.getSize(); ++j)
                {
                    printf ("%.3f ", temp[j]);
                }
                printf ("\n");
                el = el->next;
            }
        }
    }
}

/** 
 * @brief Calculate a normal for each face.
 * 
 * @param face_normals Contains the face normals, on return.
 */
void goGL::MeshObject::calculateFaceNormals (goMatrixf& face_normals) const
{
    const goMatrixf& vertices = this->getVertices();
    const goFixedArray<goVector<int> >& faces = this->getFaces();

    const goSize_t N = vertices.getRows ();
    if (N <= 0)
        return;
    const goSize_t dim = 3;
    const goSize_t N_faces = faces.getSize ();

    face_normals.resize (N_faces, dim);
    goVectorf normal;
    goVectorf temp1(0), temp2(0), temp3(0);
    for (goSize_t i = 0; i < N_faces; ++i)
    {
        face_normals.refRow (i, normal);
        if (faces[i].getSize() < 4)
        {
            vertices.refRow (faces[i][0], temp1);
            vertices.refRow (faces[i][1], temp2);
            vertices.refRow (faces[i][2], temp3);
            // (vertices[faces[i][2]] - vertices[faces[i][0]]).cross (vertices[faces[i][1]] - vertices[faces[i][0]], normal);
            (temp3 - temp1).cross (temp2 - temp1, normal);
        }
        else
        {
            vertices.refRow (faces[i][0], temp1);
            vertices.refRow (faces[i][1], temp2);
            vertices.refRow (faces[i][3], temp3);
            // (vertices[faces[i][3]] - vertices[faces[i][0]]).cross (vertices[faces[i][1]] - vertices[faces[i][0]], normal);
            (temp3 - temp1).cross (temp2 - temp1, normal);
        }
        if (normal.norm2() == 0.0f)
        {
            goString s;
            s = "goGL::MeshObject::calculateFaceNormals(): normal is 0.\n";
            s += "Face: "; s += (int)i; s += "\n";
            s += "vertices: \n";
            for (goSize_t j = 0; j < faces[i].getSize(); ++j)
            {
                s += (int)faces[i][j]; s += ": ";
                for (goSize_t k = 0; k < vertices.getColumns(); ++k)
                {
                    s += (float)vertices (faces[i][j],k);
                    s += " ";
                }
                s += "\n";
            }
            goLog::warning (s.toCharPtr());
        }
        else
        {
            normal *= 1.0 / normal.norm2();
        }
    }
}

void goGL::MeshObject::calculateNormals ()
{
    this->calculateNormals (myPrivate->normals, myPrivate->faceNormals);
}

static void check_gl_error (const char* name)
{
    if (glGetError() != GL_NO_ERROR)
    {
        printf ("%s error\n", name);
    }
}

bool goGL::MeshObject::draw () const
{
    goSize_t i = 0;
    goSize_t sz = this->getFaces().getSize(); 

    const goMatrixf& V = this->getVertices();

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
                const goVector<int>& face = this->getFaces()[i];

                glNormal3f (myPrivate->normals(face[0],0), myPrivate->normals(face[0],1), myPrivate->normals(face[0],2));
                glVertex3f (V(face[0],0), V(face[0],1), V(face[0],2));
                // glVertex3fv (this->getVertices()[face[0]].getPtr());

                glNormal3f (myPrivate->normals(face[1],0), myPrivate->normals(face[1],1), myPrivate->normals(face[1],2));
                glVertex3f (V(face[1],0), V(face[1],1), V(face[1],2));
                // glVertex3fv (this->getVertices()[face[1]].getPtr());

                glNormal3f (myPrivate->normals(face[2],0), myPrivate->normals(face[2],1), myPrivate->normals(face[2],2));
                glVertex3f (V(face[2],0), V(face[2],1), V(face[2],2));
                //glVertex3fv (this->getVertices()[face[2]].getPtr());
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
                const goVector<int>& face = this->getFaces()[i];

                glNormal3f (myPrivate->normals(face[0],0), myPrivate->normals(face[0],1), myPrivate->normals(face[0],2));
                glVertex3f (V(face[0],0), V(face[0],1), V(face[0],2));
                //glVertex3fv (this->getVertices()[face[0]].getPtr());

                glNormal3f (myPrivate->normals(face[1],0), myPrivate->normals(face[1],1), myPrivate->normals(face[1],2));
                glVertex3f (V(face[1],0), V(face[1],1), V(face[1],2));
                //glVertex3fv (this->getVertices()[face[1]].getPtr());

                glNormal3f (myPrivate->normals(face[2],0), myPrivate->normals(face[2],1), myPrivate->normals(face[2],2));
                glVertex3f (V(face[2],0), V(face[2],1), V(face[2],2));
                //glVertex3fv (this->getVertices()[face[2]].getPtr());

                glNormal3f (myPrivate->normals(face[3],0), myPrivate->normals(face[3],1), myPrivate->normals(face[3],2));
                glVertex3f (V(face[3],0), V(face[3],1), V(face[3],2));
                // glVertex3fv (this->getVertices()[face[3]].getPtr());
                ++i;
            }
            glEnd ();
        }
    }
    check_gl_error ("glend");
    return true;
}

goMatrixf& goGL::MeshObject::getNormals ()
{
    return myPrivate->normals;
}

goMatrixf& goGL::MeshObject::getFaceNormals ()
{
    return myPrivate->faceNormals;
}

const goMatrixf& goGL::MeshObject::getNormals () const
{
    return myPrivate->normals;
}

const goMatrixf& goGL::MeshObject::getFaceNormals () const
{
    return myPrivate->faceNormals;
}

/** 
 * @brief Calculate normals, create display list.
 * 
 * @return True if successful, false otherwise.
 */
bool goGL::MeshObject::init ()
{
    this->calculateNormals ();
    check_gl_error ("goGL::MeshObject::init start");
    myPrivate->listnumber = glGenLists (1);
    check_gl_error ("goGL::MeshObject::init genlists");
    return this->toList (myPrivate->listnumber);
}

bool goGL::MeshObject::writeASCII (FILE* f) const
{
    goString s = this->getFilename();
    s += "\n";
    bool ok = goFileIO::writeASCII (f, s);
    ok = ok && goGL::DrawableObject::writeASCII (f);

    return ok;
}

bool goGL::MeshObject::readASCII (FILE* f)
{
    bool ok = goFileIO::readASCIILine (f, myPrivate->filename);
    {
        //= Load and align OFF file.
        goOFFFile off;
        off.read (myPrivate->filename.toCharPtr());
        goFixedArray<goVectorf>& V = off.getVertices ();
        goSize_t sz = V.getSize ();
        goVectorf com;
        goPointCloud<goFloat>::getCenterOfMass (V, com);
        for (goSize_t i = 0; i < sz; ++i)
        {
            V[i] -= com;
        }
        off.align ();

        //= Copy vertices and faces to this object
        {
            goMatrixf& V = this->getVertices();
            this->getFaces() = off.getFaces();
            V.resize (off.getVertices().getSize(), 3);
            for (goSize_t i = 0; i < V.getRows(); ++i)
            {
                V.setRow (i, off.getVertices()[i]);
            }
        }
        //= Initialise object
        this->init ();
    }
    ok = ok && goGL::DrawableObject::readASCII (f);

    return ok;
}

/** 
 * @brief Set a filename that will be stored with \c writeASCII().
 * 
 * @param filename 
 */
void goGL::MeshObject::setFilename (const char* filename)
{
    myPrivate->filename = filename;
}

/** 
 * @brief Get the filename that will be stored with \c writeASCII().
 */
const char* goGL::MeshObject::getFilename () const
{
    return myPrivate->filename.toCharPtr ();
}
