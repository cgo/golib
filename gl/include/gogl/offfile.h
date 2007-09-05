#ifndef GOGL_OFFFILE_H
#define GOGL_OFFFILE_H

#include <goobjectbase.h>
#include <govector.h>

namespace goGL
{
    class OFFFilePrivate;

    class OFFFile : public goObjectBase
    {
        public:
            OFFFile ();
            virtual ~OFFFile ();

            bool read (const char* filename);
            bool align ();
            bool toList (int listName);
            bool draw ();

            const goVectorf& getMin () const;
            const goVectorf& getMax () const;
            // bool write (const char* filename, int listName);
            goFixedArray<goVectorf>& getVertices ();
            goFixedArray<goVector<int> >& getFaces ();

        private:
            OFFFile (OFFFile&);
            OFFFile& operator= (OFFFile&);

            goGL::OFFFilePrivate* myPrivate;
    };
};

#endif
