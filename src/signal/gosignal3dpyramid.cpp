/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#include <gosignal3dpyramid.h>
#include <assert.h>

template <class T>
Go::goSignal3DPyramid<T>::goSignal3DPyramid (goSignal3DBase<T>* signal,
                                             goIndex_t          stages)
    : goObjectBase (),
      mySignals    (NULL),
      myStages     (0)
{
    goIndex_t i;
    mySignals = new goSignal3DBase<T>* [stages];

    assert (mySignals);
    
    myStages     = stages;
    mySignals[0] = signal;
    
    goIndex_t x = signal->getSizeX ();
    goIndex_t y = signal->getSizeY ();
    goIndex_t z = signal->getSizeZ ();
    
    for (i = 1; i < stages; ++i)
    {
         x <<= 1;  if (x <= 0) x = 1;
         y <<= 1;  if (y <= 0) y = 1;
         z <<= 1;  if (z <= 0) z = 1;
         
         mySignals[i] = new goSignal3D<T>;
         mySignals[i]->make (x, y, z, 16, 16, 16, 4, 4, 4);
    }
    
    
}

