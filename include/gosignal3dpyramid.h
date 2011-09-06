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

#ifndef GOSIGNAL3DPYRAMID_H
#define GOSIGNAL3DPYRAMID_H

#include <goobjectbase.h>
#include <gotypes.h>

namespace Go {

class goSignal3DBase; 

template <class T> class
goSignal3DPyramid : public goObjectBase
{
	public:
        goSignal3DPyramid (goSignal3DBase<T>* signal, goIndex_t stages);
        virtual ~goSignal3DPyramid ();

	protected:
	
    private:
        goSignal3DBase<T>** mySignals;
        goIndex_t           myStages;

	private:
        goSignal3DPyramid (goSignal3DPyramid&);
        goSignal3DPyramid& operator= (goSignal3DPyramid&);
};
};

#endif

