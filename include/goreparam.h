#ifndef GOREPARAM_H
#define GOREPARAM_H

#include <gomatrix.h>
#include <govector.h>

namespace goMath
{
    template <class T> class ReparamPrivate;

    template <class T> class Reparam
    {
        public:
            Reparam ();
            virtual ~Reparam ();
            
            static bool equidistant (goMatrix<T>& curve, goSize_t maxIter, bool closed = true);
    };

};

#endif
