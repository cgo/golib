/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 * $Log: gofilter3d.h,v $
 * Revision 1.1.1.1  2006-04-19 15:27:10  gosch
 * golib local cvs
 *
 */

#ifndef GOFILTER3D_H
#define GOFILTER3D_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif


/*!
 * \addtogroup signal
 * @{
 */
/**
 * @brief 3D filter class.
 *
 * Applies a 3D filter to a goSignal3DBase.
 * There is a different implementation for goFloat and goDouble
 * input- and output-types (and vice versa) and for <void,void> template
 * parameters. In the <void,void> case, goSignal3DBase<void> input signals
 * can be filtered. Their use is recommended for the flexibility in data types.
 * The filtering process is a bit slower though.
 * The filter mask is currently always of type goFloat and must be given as
 * a goSignal3DBase<void>.
 * Currently, only the version for <void,void> data types has been
 * tested. It is recommended to use goSignal3D of type
 * <void> everywhere possible to remain more flexible.
 * This filter class does not support separable filters by its own.
 * Either use two or more different masks to achieve the speed gain
 * of a separable filter,
 * or implement another filter class.
 *
 * @todo Implement a separable filter class.
 * 
 * \author Christian Gosch
 **/
template<class T_IN, class T_OUT>
class
goFilter3D : public goObjectBase
{
    public:
        
	public:
        goFilter3D ();
        virtual ~goFilter3D ();
        goFilter3D (const goFilter3D<T_IN, T_OUT>& other);
        const goFilter3D& operator= (const goFilter3D<T_IN, T_OUT>& other);

        bool                      setMask (const goSignal3DBase<void>& mask,
                                           bool normalize = true, goDouble factor = 0.0);
        bool                      setMask (const goFloat* mask, goIndex_t sizeX, goIndex_t sizeY, goIndex_t sizeZ, bool normalize = true, goDouble factor = 0.0);
        void                      setMaskCenter (goIndex_t x,
                                                 goIndex_t y,
                                                 goIndex_t z);
        goIndex_t                 getMaskCenterX () const;
        goIndex_t                 getMaskCenterY () const;
        goIndex_t                 getMaskCenterZ () const;
                                                
        const goSignal3D<void>&   getMask () const;
        bool                      filter  (goSignal3DBase<T_IN>&  inSignal,
                                           goSignal3DBase<T_OUT>& outSignal);

	private:
        goSignal3D<void>            myMask;
        goIndex_t                   myMaskCenterX;
        goIndex_t                   myMaskCenterY;
        goIndex_t                   myMaskCenterZ;
};
/*! @} */
#endif

