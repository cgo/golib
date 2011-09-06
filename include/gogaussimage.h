/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGAUSSIMAGE_H
#define GOGAUSSIMAGE_H

#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif

class goGaussImagePrivate;

class goGaussImage
{
    public:
        goGaussImage ();
        virtual ~goGaussImage ();

        void update (const goSignal3DBase<void>& u, int k = -1);
        void reset  ();
        const goSignal3DBase<void>& getMean () const;
        const goSignal3DBase<void>& getVariance () const;

    private:
        goGaussImagePrivate* myPrivate;

    private:
        goGaussImage (goGaussImage&);
        goGaussImage& operator= (goGaussImage&);
};

#endif
