/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 * $Log: gosignal3dbase.h,v $
 * Revision 1.7  2003/12/30 02:56:26  christian
 * *** empty log message ***
 *
 */

#ifndef GOSIGNAL3DBASE_H
#define GOSIGNAL3DBASE_H

#include <goobjectbase.h>
#include <assert.h>

#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GOTYPE_H
# include <gotype.h>
#endif
#include <gosignal3diterator.h>
#include <gosignal3dgenericiterator.h>

/*!
 * @addtogroup signal
 * @{
 */
/*!
 * \brief Base class for up to 3D signals.
 *
 * This can be used as a template class as you would expect, giving
 * the data type as template parameter.
 * Alternatively, giving <code>void</code> as template parameter,
 * goSignal3DBase and its derivatives can be used as a generic
 * signal container with variable data type. This
 * behaviour is preferable in many situations.
 * \todo The <code>void</code> implementation is not completely done.
 *       Add support in gosignalmacros.h and add some of the routines
 *       in gosignal3d and base marked as "not yet implemented".
 *
 * \author Christian Gosch
 */
template <class T>
class
goSignal3DBase : public goObjectBase
{
    public:
        typedef goSignal3DIterator<T> iterator;
        typedef const goSignal3DIterator<T> const_iterator;

    public:
        virtual ~goSignal3DBase ();

    protected:
        goSignal3DBase ();
        goSignal3DBase (goSize_t x, goSize_t y, goSize_t z,
                goSize_t blocksize_x = 32, goSize_t blocksize_y = 32, goSize_t blocksize_z = 32,
                goSize_t border_x = 0, goSize_t border_y = 0, goSize_t border_z = 0);
        goSignal3DBase (goSignal3DBase<T>& other);
        bool     initializeDataType ();

        bool     initialize (T* dataptr,
                             goSize_t x, goSize_t y, goSize_t z,
                             goSize_t blocksize_x = 32, 
                             goSize_t blocksize_y = 32, 
                             goSize_t blocksize_z = 32,
                             goSize_t border_x    = 0, 
                             goSize_t border_y    = 0, 
                             goSize_t border_z    = 0);

    public:
        virtual void destroy ();
        
        // From goObjectInfo
        virtual goSize_t memoryUsage();

        void          setChanged  ();
        
        void          setPtr      (T *p); 
        const goType& getDataType () const;

        // Works only for <void> instantiation.
        bool setDataType (goTypeEnum t);
        
              T* getPtr ();
        const T* getPtr () const;

        const T* getRealPtr () const;
              T* getRealPtr ();

              T* getPtr     (goIndex_t x, goIndex_t y, goIndex_t z);
        const T* getPtr     (goIndex_t x, goIndex_t y, goIndex_t z) const;

        const goPtrdiff_t* getXDiff () const;
        const goPtrdiff_t* getYDiff () const;
        const goPtrdiff_t* getZDiff () const;
              goPtrdiff_t* getXDiff ();
              goPtrdiff_t* getYDiff ();
              goPtrdiff_t* getZDiff ();
        const goPtrdiff_t* getXJump () const;  
        const goPtrdiff_t* getYJump () const;  
        const goPtrdiff_t* getZJump () const;  
              goPtrdiff_t* getXJump ();  
              goPtrdiff_t* getYJump ();  
              goPtrdiff_t* getZJump ();  

        void setSize (goSize_t x,goSize_t y,goSize_t z);

        void setSize (const goSize3D& sz);

        void setSizeX(goSize_t s); 
        void setSizeY(goSize_t s); 
        void setSizeZ(goSize_t s); 

        /*!
         * \return Size in samples in x direction.
         */
        goSize_t getSizeX      () const;
        /*!
         * \return Size in samples in y direction.
         */
        goSize_t getSizeY      () const;
        /*!
         * \return Size in samples in z direction.
         */
        goSize_t getSizeZ      () const;

        goIndex_t getBorderX    () const;
        goIndex_t getBorderY    () const;
        goIndex_t getBorderZ    () const;

        goSize_t getBlockSizeX () const;
        goSize_t getBlockSizeY () const;
        goSize_t getBlockSizeZ () const;
        /*!
         * Does <strong>not</strong> perform a deep copy, instead copies size and pointer difference
         * values and the <strong>pointer</strong> to the signal data.
         */
        virtual const goSignal3DBase&	operator= (goSignal3DBase &other);

        /*!
         * "Deep" comparison of the actual data.
         */ 
        bool		operator== (goSignal3DBase &other);

        /*!
         * @return The size of the object data (without the object overhead) in bytes.
         */
        goSize_t	getSize ();

        goDouble	getMaximum() const;
        goDouble	getMinimum() const;
        void	fill (const T* value);

        /// Copies the last valid values from the block data into the borders
        // void  interpolateBorders ();

        /*! copies a side from the other signal in the border of this signal (border = 1)
         *  If you want to copy all sides and take the edges into account,
         *  take the order LEFT RIGHT TOP BOTTOM FRONT BACK to copy.
         *  See source code for details.
         */
        // void  interpolateFromSignal (goSignal3DBase<T>& other, Neighbour n);

        void shiftLeftDiff  (int n);
        void shiftRightDiff (int n);
        void shiftLeftSize  (int n);
        void shiftRightSize (int n);

        /*!
         * Not threadsafe
         */
        void rotateAxes ();
        void swapXY     ();

        const T*	  getClosest (go3Vector<goFloat>& point) const;
        goFloat    sample (go3Vector<goFloat>& point);

    protected:
        /* pointer to the first value */
        T		*ptr;
        /* pointer to the first allocated data element */
        T		*real_ptr;
        goPtrdiff_t* xDiff;
        goPtrdiff_t* yDiff;
        goPtrdiff_t* zDiff;

        goPtrdiff_t* myXJump;
        goPtrdiff_t* myYJump;
        goPtrdiff_t* myZJump;

        goSize3D     mySize;
        goSize3D     myBorderSize;
        goSize3D     myBlockSize; 
        goSize3D     myBlocks; 

        goType       myDataType;
};

#define SIGNAL3D_bilinear(__A, __B, __C, __D, __px, __py, __target) {  \
    goFloat __p1 = __A + ((__B - __A)*__px);				\
    goFloat __p2 = __C + ((__D - __C)*__px);				\
    __target =  (__p1 + ((__p2 - __p1)*__py));				\
}
/*! @} */
#endif

