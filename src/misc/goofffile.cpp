#include <goofffile.h>
#include <gofileio.h>
#include <gopointcloud.h>

#include <gotypes.h>

#include <gosort.h>

// #include <govector.h>

class goMeshEdge
{
    public:
        goMeshEdge (goIndex_t i=0, goIndex_t j=0)
            : a(i), b(j)
        {};

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

    this->removeDoubles ();
    return true;
    // return goFixMeshDirection<goFloat> (myPrivate->vertices, myPrivate->faces);
}

bool goOFFFile::removeDoubles ()
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
            int neigh1 = (j + 1) % Nv;
            int neigh2 = (j - 1);
            if (neigh2 < 0)
                neigh2 += Nv;
            if (!ret[v[j]].contains(neigh1)) 
            {
                ret[v[j]].append(neigh1);
            }
            if (!ret[v[j]].contains(neigh2)) 
            {
                ret[v[j]].append(neigh2);
            }

//            for (int k = 0; k < Nv; ++k)
//            {
//                if (j != k && !ret[v[j]].contains(v[k])) 
//                {
//                    ret[v[j]].append(v[k]);
//                }
//            }
        }
    }
}

/** 
 * @brief For each vertex, compute the list of faces it is contained in.
 * 
 * @param ret For each vertex, contains the list of faces the vertex is contained in.
 */
void goOFFFile::getAdjacentFaces (goFixedArray<goList<int> >& ret) const
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
        goLog::error ("goOFFFile::calculateNormals(): adjacency size != vertex count");
        return;
    }

#if 0
    //= Zweiter Versuch: gehe immer zu gemeinsamem, noch nicht besuchtem Nachbarn.
    {
        for (goSize_t i = 0; i < N; ++i)
        {
            goSize_t vertexCount = adj[i].getSize();
            if (faceCount == 0 || vertexCount == 0)
            {
                goLog::warning ("goOFFFile::calculateNormals(): adjFaces or adj is empty.");
                continue;
            }

            int _verticesVisited[vertexCount];
            goFixedArray<int> verticesVisited (_verticesVisited, vertexCount, 1);
            verticesVisited.fill (-1);

            int p = (int)i;
            int f = adjFaces[i].getFront();  // Start with the first face in the adjacency list
            int k = adj[i].getFront();
            verticesVisited[0] = k;
            {
                //= Find a common adjacent vertex of 
                goList<int>::Element* adjEl = adj[i].getFrontElement();
                while (adjEl)
                {
                    goList<int>::Element* el = adj[adjEl->elem].getFrontElement();
                    while (el)
                    {

                    }
                    adjEl = adjEl->next;
                }
            }

        }
    }
#endif

#if 0
    {
        goFloat _temp_vec[3];
        goVectorf temp_vec (_temp_vec, 3, 1);
        temp_vec.fill (0.0f);
        goFloat temp_count = 0.0f;
        for (goSize_t i = 0; i < N; ++i)
        {
            goSize_t faceCount = adjFaces[i].getSize();
            goSize_t vertexCount = adj[i].getSize();
            if (faceCount == 0 || vertexCount == 0)
            {
                goLog::warning ("goOFFFile::calculateNormals(): adjFaces or adj is empty.");
                continue;
            }

            int _facesVisited[faceCount];
            goFixedArray<int> facesVisited (_facesVisited, faceCount, 1);
            facesVisited.fill (-1);
            
            int _verticesVisited[vertexCount];
            goFixedArray<int> verticesVisited (_verticesVisited, vertexCount, 1);
            verticesVisited.fill (-1);
            
            int p = (int)i;
            int f = adjFaces[i].getFront();  // Start with the first face in the adjacency list
            int k = -1;
            
            int lf = 0;
            for (int lv = 0; lv < (int)vertexCount; ++lv)
            {
                for (goSize_t j = 0; j < faces[f].getSize(); ++j)
                {
                    if (!contains (verticesVisited, faces[f][j]) && faces[f][j] != i && adj[i].contains (faces[f][j]))
                    {
                        k = faces[f][j];
                        break;
                    }
                }

                verticesVisited[lv] = k;
                facesVisited[lf] = f;
                ++lf;
                //= ATTENTION lf may not by larger than faceCount-1!
                goList<int>::Element* el = adjFaces[i].getFrontElement();
                bool foundone = false;
                while (el)
                {
                    if (!contains (facesVisited, el->elem))
                    {
                        if (contains<int> (faces[el->elem], k) && contains<int> (faces[el->elem], i))
                        {
                            f = el->elem;
                            foundone = true;
                            break;
                        }
                    }
                    el = el->next;
                }
                if (!foundone)
                {
                    printf ("Did not find one. lv == %d, lf == %d\n", lv, lf);
                    break;
                }
            }
            printf ("Visited vertices: ");
            for (goSize_t j = 0; j < verticesVisited.getSize(); ++j)
                printf ("%d ", verticesVisited[j]);
            printf ("\n");
            printf ("Visited faces: ");
            for (goSize_t j = 0; j < facesVisited.getSize(); ++j)
                printf ("%d ", facesVisited[j]);
            printf ("\n");

            normal.fill (0.0f);
            temp_count = 0.0f;
            for (goSize_t j = 0; j < vertexCount - 1 && verticesVisited[j] >= 0 && verticesVisited[j+1] >= 0; ++j)
            {
                (vertices[i] - vertices[verticesVisited[j]]).cross (vertices[i] - vertices[verticesVisited[j + 1]], temp_vec);
                normal += temp_vec;
                temp_count += 1.0f;
            }
            if (temp_count > 0.0f)
            {
                normal /= temp_count;
                normal /= normal.norm2();
            }
            normals.setRow (i, normal);
            printf ("Normal: ");
            normal.print ();
        }
    }
#endif

#if 1
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
//            if (normal * fn_row < 0.0)
//            {
//                normal -= fn_row; // / fn_row.norm2();
//            }
//            else
//            {
                normal += fn_row; // / fn_row.norm2();
//            }
            el = el->next;
        }
#if 0
        for (goSize_t j = 0; j < N_faces; ++j)
        {
            for (goSize_t k = 0; k < faces[j].getSize(); ++k)
            {
                if (faces[j][k] == i)
                {
                    face_normals.refRow (j, fn_row);
                    normal += fn_row;
                    n += 1.0;
                    break;
                    if (normal * fn_row >= 0.0)
                    {
                        normal -= fn_row;
                        n += 1.0;
                    }
                    else
                    {
                        normal += fn_row;
                        n += 1.0;
                    }
                }
            }
        }
#endif
        if (normal.norm2() > 0.0f)
            normal *= 1.0f / normal.norm2();
        else
        {
            printf ("goOFFFile: ***************** NORMAL WAS 0 !!!! *********************\n");
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
#endif
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
    const goSize_t dim = 3;
    const goSize_t N_faces = faces.getSize();

    face_normals.resize (N_faces, dim);
    goVectorf normal;
    for (goSize_t i = 0; i < N_faces; ++i)
    {
        face_normals.refRow (i, normal);
        if (faces[i].getSize() < 4)
        {
            (vertices[faces[i][2]] - vertices[faces[i][0]]).cross (vertices[faces[i][1]] - vertices[faces[i][0]], normal);
        }
        else
        {
            (vertices[faces[i][3]] - vertices[faces[i][0]]).cross (vertices[faces[i][1]] - vertices[faces[i][0]], normal);
        }
        if (normal.norm2() == 0.0f)
        {
            goString s;
            s = "goOFFFile::calculateFaceNormals(): normal is 0.\n";
            s += "Face: "; s += (int)i; s += "\n";
            s += "vertices: \n";
            for (goSize_t j = 0; j < faces[i].getSize(); ++j)
            {
                s += (int)faces[i][j]; s += ": ";
                for (goSize_t k = 0; k < vertices[faces[i][j]].getSize(); ++k)
                {
                    s += (float)vertices[faces[i][j]][k];
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
