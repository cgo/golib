#ifndef GOTRIANGLE_H
#define GOTRIANGLE_H

template <class T>
class goTriangle
{
    public: 
        goTriangle(goVector<T>* p1, goVector<T>* p2, goVector<T>* p3)
            : myVertices (3), myNeighbours (3), myAdjVertices (3)
        {
            myVertices[0] = p1;
            myVertices[1] = p2;
            myVertices[2] = p3;
            myNeighbours.fill (0);
            myAdjVertices.fill (0);
        };
        ~goTriangle () {};

        goIndex_t& operator[] (goIndex_t i) { return myVertices[i]; };
        goIndex_t operator[] (goIndex_t i) const { return myVertices[i]; };
        
        goTriangle*       neigh (goIndex_t i) { return myNeighbours[i]; };
        const goTriangle* neigh (goIndex_t i) const { return myNeighbours[i]; };
        void              setNeigh (goIndex_t i, goTriangle* t) { myNeighbours[i] = t; };

    private:
        goFixedArray<goVector<T>*>   myVertices;
        goFixedArray<goTriangle<T>*> myNeighbours;
        goFixedArray<goVector<T>*>   myAdjVertices;
};

template <class T> class goTriangleMeshPrivate;

template <class T>
class goTriangleMesh
{
    public:
        goTriangleMesh ();
        virtual ~goTriangleMesh ();

    private:
        goTriangleMeshPrivate<T>* myPrivate;
};

#endif
