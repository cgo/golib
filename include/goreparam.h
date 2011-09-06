/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
