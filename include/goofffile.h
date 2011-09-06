/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif

class goOFFFilePrivate;

class goOFFFile : public goObjectBase
{
    public:
        goOFFFile ();
        virtual ~goOFFFile ();

        virtual bool read (const char* filename);
        bool removeDoubles ();
        bool align ();
        // bool toList (int listName);
        // bool draw ();

        const goMath::Vectorf& getMin () const;
        const goMath::Vectorf& getMax () const;
        // bool write (const char* filename, int listName);
        goFixedArray<goMath::Vectorf>& getVertices ();
        goFixedArray<goMath::Vector<int> >& getFaces ();
        const goFixedArray<goMath::Vectorf>& getVertices () const;
        const goFixedArray<goMath::Vector<int> >& getFaces () const;
        void getAdjacencyLists (goFixedArray<goList<int> >& ret) const;
        void getAdjacentFaces (goFixedArray<goList<int> >& ret) const;
       
        void calculateNormals (goMath::Matrixf& normals) const;
        void calculateFaceNormals (goMath::Matrixf& face_normals) const;

    private:
        goOFFFile (goOFFFile&);
        goOFFFile& operator= (goOFFFile&);

        goOFFFilePrivate* myPrivate;
};

#endif
