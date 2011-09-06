/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
            goList<goMath::Matrix<goDouble> >& getContours ();
        private:
            ContoursPrivate* myPrivate;
    };
};

#endif
