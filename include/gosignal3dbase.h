/*
 * This file and the programs contained in it and in associated files
 * are copyright by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 * $Log: gosignal3dbase.h,v $
 * Revision 1.2  2006/04/21 18:39:34  gosch
 * *** empty log message ***
 *
 * Revision 1.1.1.1  2006/04/19 15:27:06  gosch
 * golib local cvs
 *
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
#ifndef GODEFS_H
# include <godefs.h>
#endif
#ifndef GOFIXEDARRAY_H
# include <gofixedarray.h>
#endif

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
 *
 * This class can handle multichannel data (such as RGBA).
 * To select a channel, use <code>setChannel()</code>.
 * All data access functions will then work so that
 * any accessed data value is from the selected channel.
 * For details on how to create a multichannel see <code>goSignal3D</code>
 * 
 * \note Although instances for many explicit data types
 *       are provided, it is recommended to use the special void type instantiation.
 * 
 * \todo The <code>void</code> implementation may not completely be done.
 *       Check if some of the routines
 *       in gosignal3d and base are still marked as "not yet implemented".
 * \todo typedef the void instantiation to something else and use that.
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
                        goSize_t blocksize_x = 32, 
                        goSize_t blocksize_y = 32, 
                        goSize_t blocksize_z = 32,
                        goSize_t border_x = 16, 
                        goSize_t border_y = 16, 
                        goSize_t border_z = 16,
                        goSize_t channelCount = 1);
        goSignal3DBase (goSignal3DBase<T>& other);
        bool     initializeDataType ();

        bool     initialize (T* dataptr,
                             goSize_t x, goSize_t y, goSize_t z,
                             goSize_t blocksize_x = 32, 
                             goSize_t blocksize_y = 32, 
                             goSize_t blocksize_z = 32,
                             goSize_t border_x    = 16, 
                             goSize_t border_y    = 16, 
                             goSize_t border_z    = 16,
                             goSize_t channelCount = 1);

        void     cleanup ();

    public:
        virtual void destroy ();
        
        // From goObjectInfo
        virtual goSize_t memoryUsage() const;

        void          setChanged  ();
        
        void          setPtr      (T *p); 
        const goType& getDataType () const;
        
        void          setChannel  (goSize_t c);
        goSize_t      getChannel  () const { return this->myChannel; };

        /** 
         * @brief Get the pointer offset from a channel 0 element to
         * the corresponding channel <code>channel</code> element.
         * 
         * @param channel Channel to which the pointer offset is wanted.
         * 
         * @return Pointer offset to <code>channel</code> element.
         */
        goPtrdiff_t   getChannelOffset (goSize_t channel) const 
        {
            assert (channel < myChannelCount);
            return myChannelOffset[channel];
        };

        virtual void             setBorderFlags (int axes = GO_X|GO_Y|GO_Z, int borderFlag = GO_PERIODIC_BORDER);
        virtual void             setBorderFlags (const goFixedArray<int>& flags);
        const goFixedArray<int>& getBorderFlags () const;
        
        // Works only for <void> instantiation. You should always use void where
        // there is no good reason to use another type. Void rules.
        bool setDataType (goTypeEnum t);

        goDouble getValue (goIndex_t x, goIndex_t y = 0, goIndex_t z = 0, goSize_t channel = 0) const;
        void     setValue (goDouble value, goIndex_t x, goIndex_t y = 0, goIndex_t z = 0, goSize_t channel = 0);

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

        void setSize (goSize_t x,goSize_t y,goSize_t z,goSize_t channelCount = 1);

        void setSize (const goSize3D& sz);

        void setSizeX(goSize_t s); 
        void setSizeY(goSize_t s); 
        void setSizeZ(goSize_t s); 
       
        const goSize3D& getSize () const { return mySize; };
        const goSize3D& getBlockSize () const { return myBlockSize; };
        const goSize3D& getBorderSize () const { return myBorderSize; };

        void resizeBorder (const goSize3D& size);

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

        goSize_t getChannelCount () const;

        /*!
         * Does <strong>not</strong> perform a deep copy, instead copies size and pointer difference
         * values and the <strong>pointer</strong> to the signal data.
         */
        const goSignal3DBase<T>& reference (goSignal3DBase<T> &other);

        /*!
         * "Deep" comparison of the actual data.
         */ 
        bool		operator== (goSignal3DBase<T> &other);

        goDouble	getMaximum() const;
        goDouble	getMinimum() const;
        void        getMinMax(goDouble& minRet, goDouble& maxRet) const;
        virtual void fill (const T* value);
        virtual void fill (goDouble value);

        /// Copies the last valid values from the block data into the borders
        // void  interpolateBorders ();

        /*! copies a side from the other signal in the border of this signal (border = 1)
         *  If you want to copy all sides and take the edges into account,
         *  take the order LEFT RIGHT TOP BOTTOM FRONT BACK to copy.
         *  See source code for details.
         */
        // void  interpolateFromSignal (goSignal3DBase<T>& other, Neighbour n);

        virtual void shiftLeftDiff  (int n, int axes = GO_X | GO_Y | GO_Z);
        virtual void shiftRightDiff (int n, int axes = GO_X | GO_Y | GO_Z);
        virtual void shiftLeftSize  (int n, int axes = GO_X | GO_Y | GO_Z);
        virtual void shiftRightSize (int n, int axes = GO_X | GO_Y | GO_Z);

        /*!
         * Not threadsafe
         */
        void rotateAxes ();
        void swapXY     ();
        void flip       (int axis);

        const T*   getClosest (go3Vector<goFloat>& point) const;
        goFloat    sample (go3Vector<goFloat>& point);

        goDouble sum () const;

        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels,
         * up to the minimum number of channels of this object and the other object
         * 
         * @param other Signal to combine with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator += (const goSignal3DBase<T>& other);
        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels,
         * up to the minimum number of channels of this object and the other object
         * 
         * @param other Signal to combine with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator -= (const goSignal3DBase<T>& other);
        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels,
         * up to the minimum number of channels of this object and the other object
         * 
         * @param other Signal to combine with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator *= (const goSignal3DBase<T>& other);
        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels,
         * up to the minimum number of channels of this object and the other object
         * 
         * @param other Signal to combine with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator /= (const goSignal3DBase<T>& other);
        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels.
         * 
         * @param other Signal to combine with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator += (goFloat scalar);
        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels.
         * 
         * @param other Signal to combine with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator -= (goFloat scalar);
        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels.
         * 
         * @param scalar Scalar to multiply each value in this signal with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator *= (goFloat scalar);
        /** 
         * @brief Element-wise operator.
         *
         * All operator [/+-*]= work on all channels.
         * 
         * @param other Signal to combine with.
         * 
         * @return Reference to *this.
         */
        goSignal3DBase<T>& operator /= (goFloat scalar);
        
        void applyBorderFlags (int axis);
        void applyBorderFlags ();

    protected:
        void setBorder (goSize_t x, goSize_t y, goSize_t z);
        void constantBorders (int axes = GO_X|GO_Y|GO_Z);
        void periodicBorders (int axes = GO_X|GO_Y|GO_Z);
        
    protected:
        /* pointer to the first value */
        T		*ptr;
        /* pointer to the first allocated data element */
        T		*real_ptr;
        goPtrdiff_t* xDiff;
        goPtrdiff_t* yDiff;
        goPtrdiff_t* zDiff;
        goPtrdiff_t* real_xDiff;
        goPtrdiff_t* real_yDiff;
        goPtrdiff_t* real_zDiff;

        goPtrdiff_t* myXJump;
        goPtrdiff_t* myYJump;
        goPtrdiff_t* myZJump;
        goPtrdiff_t* real_myXJump;
        goPtrdiff_t* real_myYJump;
        goPtrdiff_t* real_myZJump;
        
        goPtrdiff_t* myChannelOffset;
        
        goSize3D     mySize;
        goSize3D     myBorderSize;
        goSize3D     myBlockSize; 
        goSize3D     myBlocks; 

        goType       myDataType;
        goSize_t     myChannelCount;
        goSize_t     myChannel;

        goFixedArray<int> myBorderFlags;

    private:
        goSignal3DBase<T>& operator= (goSignal3DBase<T>&);
};

#define SIGNAL3D_bilinear(__A, __B, __C, __D, __px, __py, __target) {  \
    goFloat __p1 = __A + ((__B - __A)*__px);				\
    goFloat __p2 = __C + ((__D - __C)*__px);				\
    __target =  (__p1 + ((__p2 - __p1)*__py));				\
}
/*! @} */
#endif

