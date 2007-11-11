#ifndef GOOFFFILE_H
#define GOOFFFILE_H

#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif

class goOFFFilePrivate;

class goOFFFile : public goObjectBase
{
    public:
        goOFFFile ();
        virtual ~goOFFFile ();

        bool read (const char* filename);
        bool align ();
        // bool toList (int listName);
        // bool draw ();

        const goVectorf& getMin () const;
        const goVectorf& getMax () const;
        // bool write (const char* filename, int listName);
        goFixedArray<goVectorf>& getVertices ();
        goFixedArray<goVector<int> >& getFaces ();
        const goFixedArray<goVectorf>& getVertices () const;
        const goFixedArray<goVector<int> >& getFaces () const;
        void getAdjacencyLists (goFixedArray<goList<int> >& ret) const;
       

    private:
        goOFFFile (goOFFFile&);
        goOFFFile& operator= (goOFFFile&);

        goOFFFilePrivate* myPrivate;
};

#endif
