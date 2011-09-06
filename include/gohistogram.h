/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gohistogram.h,v 1.1.1.1 2006/04/19 15:27:04 gosch Exp $
 */

#ifndef GOHISTOGRAM_H
#define GOHISTOGRAM_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOARRAY_H
# include <goarray.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOTYPE_H
# include <gotype.h>
#endif

template <class level_type> class goHistogramPrivate;

/**
 * @addtogroup signal
 */
/** @{ */
template <class level_type>
class
goHistogram : public goObjectBase
{
	public:
        goHistogram ();
        virtual ~goHistogram ();

        goHistogram (const goHistogram<level_type>& other);
        const goHistogram<level_type>& operator= (const goHistogram<level_type>& other);

        goDouble             getMinValue  () const;
        goDouble             getBinStep   () const;

        virtual bool         calculate    (const goSignal3DBase<void>&, bool normalize = false);
        bool                 setLevels    (const goArray<level_type>& levelArray);
        bool                 setBins      (goSize_t n);
        goIndex_t            getBins      () const;
        goDouble             lookup       (const void* valueP);
        goArray<goDouble>&   getHistogram ();
        goArray<level_type>& getLevels    ();
        
	protected:
        virtual bool calculateCore (const goSignal3DBase<void>& sig);
	
	private:
        goHistogramPrivate<level_type>* myPrivate;

    private:
        goHistogram<level_type>& operator= (goHistogram<level_type>&);
        goHistogram (goHistogram<level_type>&);
};
/** @} */

class goCDFPrivate;

/**
 * @addtogroup signal
 */
/** @{ */
template <class level_type>
class
goCDF : public goHistogram<level_type>
{
    public:
        goCDF ();
        virtual ~goCDF ();

        virtual bool calculate      (const goSignal3DBase<void>&, bool normalize = true);
        bool         makeInverseLUT (goArray<goFloat>& inverseLUTRet, goIndexFunction& indexFunctionRet);
    private:
        goCDFPrivate* myPrivate;
};
/** @} */

bool goMatchHistograms (const goSignal3DBase<void>* fromSignal, const goSignal3DBase<void>* toSignal, goSignal3DBase<void>* retSignal);

template <class T>
bool goEqualizeHistogram (goSignal3DBase<void>* sig, goCDF<T>& targetCDF);

#endif
