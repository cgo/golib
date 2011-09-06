/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOSIGNAL_H
# include <gosignal.h>
#endif

#include <gofilter1d.h>
#include <gosignal3dbase.h>

bool goSignal::smooth (goSignal3DBase<void>& sig, goSize_t width)
{
    goFloat mask[width];
   
    goFloat* mp = mask;
    for (goSize_t i = 0; i < width; ++i, ++mp)
        *mp = 1.0f;

    goFilter1D filter (mask, width, width >> 1, true);
    if (sig.getSizeX() > 1 && sig.getBorderX() >= width)
    {
        filter.filter (sig);
    }
    if (sig.getSizeY() > 1 && sig.getBorderY() >= width)
    {
        sig.rotateAxes ();
        sig.rotateAxes ();
        filter.filter (sig);
        sig.rotateAxes ();
    }
    if (sig.getSizeZ() > 1 && sig.getBorderZ() >= width)
    {
        sig.rotateAxes ();
        filter.filter (sig);
        sig.rotateAxes ();
        sig.rotateAxes ();
    }

    return true;
}
