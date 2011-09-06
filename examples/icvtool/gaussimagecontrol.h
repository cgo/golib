/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GAUSSIMAGECONTROL_H
#define GAUSSIMAGECONTROL_H


#include <gosignal3dbase.h>
#include <goautoptr.h>
#include <gogui/control.h>

class GaussImageControlPrivate;

class GaussImageControl : public goGUI::Control
{
    public:
        GaussImageControl ();
        virtual ~GaussImageControl ();

        void update (goAutoPtr<goSignal3DBase<void> >  image);
        void difference (goAutoPtr<goSignal3DBase<void> >  image);

    private:
        GaussImageControlPrivate *myPrivate;
};

#endif
