/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gofilter1d.h,v 1.1.1.1 2006/04/19 15:27:10 gosch Exp $
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

/*!
 * \addtogroup signal
 * @{
 */

/**
 * @brief Separable filter for 3D signals.
 *
 * Applies a separable filter to a goSignal3DBase<void>. Note that
 * this filter class was not implemented for typed instances of goSignal3DBase.
 * You should not use typed versions anymore anyway.
 *
 * Usage would be like this:
 * <pre>
 *  <code>
 *      ...
 *      goSignal3D<void> sig;
 *      ...
 *      goArray<goFloat> mask;
 *      mask += 1.0f;
 *      mask += 2.0f;
 *      mask += 1.0f;
 *      goFilter1D filter;
 *      filter.setMask   (mask);  // Set the mask
 *      filter.setCenter (1);     // Set center to the entry "2"
 *      filter.normalize ();      // Normalize the mask
 *      filter.filter    (sig);   // Filter in-place
 *      ...
 *  </code>
 * </pre>
 * 
 * @return 
 **/
class
goFilter1D : public goObjectBase
{
	public:
	    goFilter1D ();
        goFilter1D (const goArray<goFloat>& mask, goIndex_t center = 0, bool normalize = true);
        goFilter1D (const goFloat* mask, goIndex_t length, goIndex_t center = 0, bool normalize = true);
        virtual ~goFilter1D ();
    
        bool setMask   (const goArray<goFloat>& mask);
        bool setMask   (const goFloat* mask, goIndex_t length);
        bool setCenter (goIndex_t c);
        bool filter    (goSignal3DBase<void>& sig);
        bool normalize (goDouble constant = 1.0);
        
	private:
        goFilter1DPrivate* myPrivate;
};
/*! @} */
#endif

