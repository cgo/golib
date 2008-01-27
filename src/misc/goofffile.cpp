#include <goofffile.h>
#include <gofileio.h>
#include <gopointcloud.h>

#include <gosort.h>

class goMeshEdge
{
    public:
        goMeshEdge (goIndex_t i=0, goIndex_t j=0)
            : a(i), b(j)
        {
        };

        ~goMeshEdge () {};
        
        void swap ()
        {
            goIndex_t t = this->a;
            this->a = this->b;
            this->b = t;
        };

        goIndex_t key () const { return goMath::min<goIndex_t> (a,b); };
        
        bool operator== (const goMeshEdge& o) const
        {
            return goMath::min<goIndex_t> (this->a,this->b) == goMath::min<goIndex_t> (o.a,o.b) &&
                goMath::max<goIndex_t> (this->a,this->b) == goMath::max<goIndex_t> (o.a,o.b);
        };

        bool operator!= (const goMeshEdge& o) const { return !(*this == o); };

        goIndex_t a;
        goIndex_t b;
};

#include <gohashtable.h>
#ifndef GOHASHTABLE_HPP
# include <gohashtable.hpp>
#endif

template <class T>
bool goFixMeshDirection (goFixedArray<goVector<T> >& vertices, goFixedArray<goVector<int> >& faces)
{
    goSize_t sz = faces.getSize();
    if (sz == 0)
        return true;

    goHashTable<goIndex_t, goMeshEdge> edgeTable;
    
    goSize_t edgesInTable = 0;
    goSize_t sz_f = faces[0].getSize();
    for (goSize_t i = 0; i < sz_f; ++i)
    {
        goMeshEdge e (faces[0][i],faces[0][(i+1) % sz_f]);
        edgeTable.add (e.key(), e);
        ++edgesInTable;
    }

    goFixedArray<bool> facesDone(sz);
    facesDone.fill (false);
    facesDone[0] = true;
    goSize_t totalFacesDone = 1;
    bool changed = true;

    while (changed)
    {
        changed = false;
        for (goSize_t i = 1; i < sz; ++i)
        {
            //= Ist eine Kante schon in der Tabelle?
            goVector<int>& face = faces[i];
            goSize_t sz_f = face.getSize();
            for (goSize_t j = 0; j < sz_f; ++j)
            {
                //= Falls ja, face umdrehen und andere Kanten markieren (== in Tabelle einfuegen)
                goIndex_t key_bak = face[j];
                goMeshEdge& e = edgeTable[key_bak];
                if (!edgeTable.fail())
                {
                    if (!facesDone[i])
                    {
                        //= Falls edge e in face in die gleiche Richtung zeigt, umdrehen.
                        for (goSize_t k = 0; k < sz_f; ++k)
                        {
                            goMeshEdge e2 (face[k], face[(k+1) % sz_f]);
                            if (e == e2)
                            {
                                if (e.a == e2.a)
                                {
                                    face.flip();
                                    changed = true;
                                    break;
                                }
                            }
                        }
                        facesDone[i] = true;
                        ++totalFacesDone;
                        printf ("totalFacesDone == %d\n", totalFacesDone);
                        for (goSize_t k = 0; k < sz_f; ++k)
                        {
                            goMeshEdge e2 (face[k], face[(k+1) % sz_f]);
                            if (e != e2)
                            {
                                edgeTable.add (e2.key(), e2);
                                ++edgesInTable;
                                //printf ("edgesInTable == %d\n", edgesInTable);
                            }
                        }
                    }
                    //edgeTable.remove (key_bak);
                    //--edgesInTable;
                    //printf ("edgesInTable == %d\n", edgesInTable);
                    break;
                }
            }
        }
    }

    return true;
}

class goOFFFilePrivate
{
    public:
        goOFFFilePrivate () 
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
        ~goOFFFilePrivate () {};

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

goOFFFile::goOFFFile ()
    : myPrivate (0)
{
    myPrivate = new goOFFFilePrivate;
}

goOFFFile::~goOFFFile ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

bool goOFFFile::read (const char* filename)
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
            goString temp = "goOFFFile: Line ";
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
                goString temp = "goOFFFile: incorrect dimension ";
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
            goLog::warning ("goOFFFile: Expected number of vertices could not be read.");
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
                goString temp = "goOFFFile: incorrect line ";
                temp += (int) lineNumber;
                goLog::warning (temp.toCharPtr());
                ++i;
                continue;
            }
            int NV = words.getFrontElement()->elem.toInt ();
            if (words.getSize() < NV+1)
            {
                goString temp = "goOFFFile: incorrect line ";
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
    // return goFixMeshDirection<goFloat> (myPrivate->vertices, myPrivate->faces);
}

bool goOFFFile::align ()
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

const goVectorf& goOFFFile::getMin () const
{
    return myPrivate->min;
}

const goVectorf& goOFFFile::getMax () const
{
    return myPrivate->max;
}

goFixedArray<goVectorf>& goOFFFile::getVertices ()
{
    return myPrivate->vertices;
}

goFixedArray<goVector<int> >& goOFFFile::getFaces ()
{
    return myPrivate->faces;
}

const goFixedArray<goVectorf>& goOFFFile::getVertices () const
{
    return myPrivate->vertices;
}

const goFixedArray<goVector<int> >& goOFFFile::getFaces () const
{
    return myPrivate->faces;
}

/** 
 * @brief Build adjacency lists for all vertices of this object.
 *
 * @note The adjacency lists contain the graph structure only for triangular meshes.
 * For quadrilaterals, also the diagonal points are added as being adjacent.
 */
void goOFFFile::getAdjacencyLists (goFixedArray<goList<int> >& ret) const
{
    const goFixedArray<goVector<int> >& faces = this->getFaces();
    int N = this->getVertices().getSize();
    ret.setSize (0);
    ret.setSize (N);
    int Nfaces = faces.getSize();
    for (int fi = 0; fi < Nfaces; ++fi)
    {
        const goVector<int>& v = faces[fi];
        int Nv = v.getSize();
        for (int j = 0; j < Nv; ++j)
        {
            for (int k = 0; k < Nv; ++k)
            {
                if (j != k && !ret[v[j]].contains(v[k])) 
                {
                    ret[v[j]].append(v[k]);
                }
            }
        }
    }
}

/** 
 * @brief This is very slow -- calculate normals for all vertices.
 * 
 * @param normals Normals for each vertex.
 */
void goOFFFile::calculateNormals (goMatrixf& normals) const
{
    const goFixedArray<goVectorf>& vertices = this->getVertices();
    const goFixedArray<goVector<int> >& faces = this->getFaces();

    const goSize_t N = vertices.getSize();
    if (N <= 0)
        return;
    const goSize_t dim = vertices[0].getSize();
    const goSize_t N_faces = faces.getSize();

    goMatrixf face_normals (N_faces, dim);
    goVectorf normal;
    for (goSize_t i = 0; i < N_faces; ++i)
    {
        face_normals.refRow (i, normal);
        //goVector<int> temp = faces[i];
        //goSort (temp.getPtr(), temp.getSize());
        //(vertices[temp[1]] - vertices[temp[0]]).cross (vertices[temp[2]] - vertices[temp[0]], normal);
        (vertices[faces[i][2]] - vertices[faces[i][0]]).cross (vertices[faces[i][1]] - vertices[faces[i][0]], normal);
        // normal *= 1.0 / normal.norm2();
    }

    normals.resize (N, dim);
    normals.fill (0.0f);
    for (goSize_t i = 0; i < N; ++i)
    {
        goFloat n = 0.0f;
        normals.refRow (i, normal);
        for (goSize_t j = 0; j < N_faces; ++j)
        {
            for (goSize_t k = 0; k < faces[j].getSize(); ++k)
            {
                if (faces[j][k] == i)
                {
                    normal += face_normals[j];
                    n += 1.0;
                    break;
                }
            }
        }
        if (normal.norm2() > 0.0f)
            normal *= 1.0f / normal.norm2();
        else
            normal.fill (0.0f);
    }
}

/** 
 * @brief Calculate a normal for each face.
 * 
 * @param face_normals Contains the face normals, on return.
 */
void goOFFFile::calculateFaceNormals (goMatrixf& face_normals) const
{
    const goFixedArray<goVectorf>& vertices = this->getVertices();
    const goFixedArray<goVector<int> >& faces = this->getFaces();

    const goSize_t N = vertices.getSize();
    if (N <= 0)
        return;
    const goSize_t dim = vertices[0].getSize();
    const goSize_t N_faces = faces.getSize();

    face_normals.resize (N_faces, dim);
    goVectorf normal;
    for (goSize_t i = 0; i < N_faces; ++i)
    {
        face_normals.refRow (i, normal);
        (vertices[faces[i][2]] - vertices[faces[i][0]]).cross (vertices[faces[i][1]] - vertices[faces[i][0]], normal);
        normal *= 1.0 / normal.norm2();
    }
}
