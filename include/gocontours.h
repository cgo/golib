#ifndef GOCONTOURS_H
#define GOCONTOURS_H

#include <gosignal3dbase.h>
#include <gomatrix.h>

namespace goMath 
{
    class ContoursPrivate;

    class Contours
    {
        public:
            Contours ();
            virtual ~Contours ();

            void calculate (const goSignal3DBase<void>& image, goDouble level);
            goList<goMatrix<goDouble> >& getContours ();
        private:
            ContoursPrivate* myPrivate;
    };
};

#endif
