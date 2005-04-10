/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#ifndef GOFILTER1D_H
#define GOFILTER1D_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif

#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif

class goFilter1DPrivate;

class
goFilter1D : public goObjectBase
{
	public:
	    goFilter1D ();
        virtual ~goFilter1D ();
    
        bool setMask   (const goArray<goFloat>& mask);
        bool setCenter (goIndex_t c);
        bool filter    (goSignal3DBase<void>& sig);
        
	private:
        goFilter1DPrivate* myPrivate;
};

#endif

