/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
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

class goHistogramPrivate;

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

        virtual bool         calculate    (const goSignal3DBase<void>&, bool normalize = false);
        bool                 setLevels    (const goArray<level_type>& levelArray);
        bool                 setBins      (goSize_t n);
        goDouble             lookup       (const void* valueP);
        goArray<goDouble>&   getHistogram ();
        goArray<level_type>& getLevels    ();
        
        
	protected:
	
	private:
        goArray<level_type> myLevels;
        goHistogramPrivate* myPrivate;

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
        bool         makeInverseLUT (goArray<goIndex_t>& inverseLUTRet, goIndexFunction& indexFunctionRet);
    private:
        goCDFPrivate* myPrivate;
};
/** @} */

bool goMatchHistograms (const goSignal3DBase<void>* fromSignal, const goSignal3DBase<void>* toSignal, goSignal3DBase<void>* retSignal);

#endif

