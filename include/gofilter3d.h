/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 * $Log$
 */

#ifndef GOFILTER3D_H
#define GOFILTER3D_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif

template <class T> class goSignal3DBase;
template <class T> class goSignal3D;

/*!
 * \addtogroup signal
 * @{
 */
/**
 * @brief 3D filter class.
 *
 * \author Christian Gosch
 **/
template<class T_IN, class T_OUT>
class
goFilter3D : public goObjectBase
{
    public:
    typedef goFloat mask_t;
        
	public:
        goFilter3D ();
        virtual ~goFilter3D ();
        goFilter3D (const goFilter3D<T_IN, T_OUT>& other);
        const goFilter3D& operator= (const goFilter3D<T_IN, T_OUT>& other);

        bool                      setMask (const goSignal3DBase<mask_t>& mask,
                                           bool normalize = true);
        void                      setMaskCenter (goIndex_t x,
                                                 goIndex_t y,
                                                 goIndex_t z);
        goIndex_t                 getMaskCenterX () const;
        goIndex_t                 getMaskCenterY () const;
        goIndex_t                 getMaskCenterZ () const;
                                                
        const goSignal3D<mask_t>& getMask () const;
        bool                      filter  (goSignal3DBase<T_IN>&  inSignal,
                                           goSignal3DBase<T_OUT>& outSignal);

	private:
        goSignal3D<mask_t> *myMask;
        goIndex_t           maskCenterX;
        goIndex_t           maskCenterY;
        goIndex_t           maskCenterZ;
};
/*! @} */
#endif

