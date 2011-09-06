/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOIMAGE_H
#define GOIMAGE_H

#include <goimagebuffer.h>

class goImagePrivate;

class goImage : goObjectBase 
{
    public:
        goImage  ();
        virtual ~goImage ();
    
    private:
        goImagePrivate* myPrivate;
};

#endif /* GOIMAGE_H */
