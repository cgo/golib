//=
//= This is incomplete -- not needed at this point in time.
//=

#include <gotriangle.h>

template <class T> class goTriangleMeshPrivate
{
    public:
        goTriangleMeshPrivate () {};
        ~goTriangleMeshPrivate () {};

        goList<goVector<T> > vertexList;
        goList<goTriangle<T> > triangleList;
};

template <class T>
goTriangleMesh::goTriangleMesh ()
    : myPrivate (0)
{
    myPrivate = new goTriangleMeshPrivate<T>;
}

template <class T>
goTriangleMesh::~goTriangleMesh ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

//= Standard subdivision
template <class T>
bool goTriangleMesh::subdivideTriangle (goTriangle<T>* tri_)
{
    goTriangle<T>& tri = *tri_;
    
    goVector<T> *p0,*p1,*p2;
    myPrivate->vertexList.append ((*tri[1] + *tri[2]) * 0.5);
    p0 = &vertexList.getTail();
    myPrivate->vertexList.append ((*tri[0] + *tri[2]) * 0.5);
    p1 = &vertexList.getTail();
    myPrivate->vertexList.append ((*tri[0] + *tri[1]) * 0.5);
    p2 = &vertexList.getTail();
    
    myPrivate->triangleList.append (goTriangle<T>(tri[0], p2, p1));
    myPrivate->triangleList.append (goTriangle<T>(tri[1], p0, p2));
    myPrivate->triangleList.append (goTriangle<T>(tri[2], p0, p1));
    myPrivate->triangleList.append (goTriangle<T>(p0, p1, p2));

    //= Add "green" triangle pairs to replace the adjacent triangles.
    goTriangle<T>& adj = *tri.neigh(0);
    myPrivate->triangleList.append (goTriangle<T>(tri[1], p0, adj
    //= FIXME incomplete
    return true;
}

//= Subdivide "green" pair of triangles
template <class T>
bool goTriangleMesh::subdivideGreenPair (goTriangle* tri1_, goTriangle* tri2_, goVector<T>* greenVertex)
{
}

//= Subdivision of green triangle pair
