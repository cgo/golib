/*
 * This file and the programs contained in it and in associated files
 * are copyright 2002 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id: gosignal3dbase.h,v 1.5 2003/07/20 09:57:38 christian Exp $
 * $Log: gosignal3dbase.h,v $
 * Revision 1.5  2003/07/20 09:57:38  christian
 * *** empty log message ***
 *
 * Revision 1.4  2003/06/22 14:54:49  christian
 * Changes to enable sub sampling for dwt
 *
 * Revision 1.3  2002/11/01 12:46:13  christian
 * changed getBorder*() return values to goIndex_t
 *
 * Revision 1.2  2002/10/26 15:12:23  christian
 * New signal class structure
 *
 * Revision 1.1  2002/10/03 21:34:47  christian
 * *** empty log message ***
 *
 */

#ifndef GOSIGNAL3DBASE_H
#define GOSIGNAL3DBASE_H

#include <goobjectinfo.h>
#include <gotypes.h>

template <class T>
class
goSignal3DBase : public goObjectInfo
{
    public:
        virtual ~goSignal3DBase ();

    protected:
        goSignal3DBase ();
        goSignal3DBase (goSize_t x, goSize_t y, goSize_t z,
                goSize_t blocksize_x = 32, goSize_t blocksize_y = 32, goSize_t blocksize_z = 32,
                goSize_t border_x = 0, goSize_t border_y = 0, goSize_t border_z = 0);
        goSignal3DBase (goSignal3DBase<T>& other);

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

        void setPtr (T *p); 

        inline       T* getPtr ()       { return getPtr (0, 0, 0);}
        inline const T* getPtr () const { return (const T*)getPtr (0, 0, 0);}

        inline const T* getRealPtr () const { return (const T*)real_ptr; }
        inline       T* getRealPtr () { return real_ptr; }

        inline T*       getPtr     (goIndex_t x, goIndex_t y, goIndex_t z);
        inline const T* getPtr     (goIndex_t x, goIndex_t y, goIndex_t z) const;

        inline const goPtrdiff_t* getXDiff () const;
        inline const goPtrdiff_t* getYDiff () const;
        inline const goPtrdiff_t* getZDiff () const;
        inline       goPtrdiff_t* getXDiff ();
        inline       goPtrdiff_t* getYDiff ();
        inline       goPtrdiff_t* getZDiff ();
        inline const goPtrdiff_t* getXJump () const;  
        inline const goPtrdiff_t* getYJump () const;  
        inline const goPtrdiff_t* getZJump () const;  
        inline       goPtrdiff_t* getXJump ();  
        inline       goPtrdiff_t* getYJump ();  
        inline       goPtrdiff_t* getZJump ();  

        inline void setSize (goSize_t x,goSize_t y,goSize_t z)
        { 
            mySize.x = x; mySize.y = y; mySize.z = z; 
        }

        inline void setSize (const goSize3D& sz)
        {
            mySize = sz;
        }

        inline void setSizeX(goSize_t s) { mySize.x = s; }
        inline void setSizeY(goSize_t s) { mySize.y = s; }
        inline void setSizeZ(goSize_t s) { mySize.z = s; }

        /*!
         * \return Size in samples in x direction.
         */
        inline goSize_t getSizeX      () const { return mySize.x; }
        /*!
         * \return Size in samples in y direction.
         */
        inline goSize_t getSizeY      () const { return mySize.y; }
        /*!
         * \return Size in samples in z direction.
         */
        inline goSize_t getSizeZ      () const { return mySize.z; }

        inline goIndex_t getBorderX    () const { return (goIndex_t)myBorderSize.x; }
        inline goIndex_t getBorderY    () const { return (goIndex_t)myBorderSize.y; }
        inline goIndex_t getBorderZ    () const { return (goIndex_t)myBorderSize.z; }

        inline goSize_t getBlockSizeX () const { return myBlockSize.x; }
        inline goSize_t getBlockSizeY () const { return myBlockSize.y; }
        inline goSize_t getBlockSizeZ () const { return myBlockSize.z; }
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

        T	getMaximum();
        T	getMinimum();
        void	fill (T value);

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
        inline void rotateAxes ();
        inline void swapXY     ();

        inline T	  getClosest (go3Vector<goFloat>& point);
        inline goFloat sample (go3Vector<goFloat>& point);

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
};

#define SIGNAL3D_bilinear(__A, __B, __C, __D, __px, __py, __target) {  \
    goFloat __p1 = __A + ((__B - __A)*__px);				\
    goFloat __p2 = __C + ((__D - __C)*__px);				\
    __target =  (__p1 + ((__p2 - __p1)*__py));				\
}

template<class T>
inline const goPtrdiff_t* 
goSignal3DBase<T>::getXDiff () const
{
    return xDiff;
}

template<class T>
inline const goPtrdiff_t* 
goSignal3DBase<T>::getYDiff () const
{
    return yDiff;
}

template<class T>
inline const goPtrdiff_t* 
goSignal3DBase<T>::getZDiff () const
{
    return zDiff;
}

template<class T>
inline goPtrdiff_t* 
goSignal3DBase<T>::getXDiff () 
{
    return xDiff;
}

template<class T>
inline goPtrdiff_t* 
goSignal3DBase<T>::getYDiff () 
{
    return yDiff;
}

template<class T>
inline goPtrdiff_t* 
goSignal3DBase<T>::getZDiff () 
{
    return zDiff;
}

template<class T>
inline const goPtrdiff_t*
goSignal3DBase<T>::getXJump () const
{
    return myXJump;
}

template<class T>
inline const goPtrdiff_t*
goSignal3DBase<T>::getYJump () const
{
    return myYJump;
}

template<class T>
inline const goPtrdiff_t*
goSignal3DBase<T>::getZJump () const
{
    return myZJump;
}

template<class T>
inline goPtrdiff_t*
goSignal3DBase<T>::getXJump () 
{
    return myXJump;
}

template<class T>
inline goPtrdiff_t*
goSignal3DBase<T>::getYJump () 
{
    return myYJump;
}

template<class T>
inline goPtrdiff_t*
goSignal3DBase<T>::getZJump () 
{
    return myZJump;
}

#if 0
template<class T>
inline
void
goSignal3DBase<T>::shiftLeftDiffX (int n)
{
   if (n <= 0)
       return;

   if (getSizeX() > 1)
   {
       
   }
   shiftLeftDiffX (n-1);
}
#endif

template< class T >
inline
void
goSignal3DBase<T>::rotateAxes ()
{
  goPtrdiff_t* tempDiff = zDiff;
  goSize_t tempSize = mySize.z;
  goPtrdiff_t* tempJump = myZJump;
  
  mySize.z = mySize.y;
  mySize.y = mySize.x;
  mySize.x = tempSize;
  zDiff = yDiff;
  yDiff = xDiff;
  xDiff = tempDiff;
  myZJump = myYJump;
  myYJump = myXJump;
  myXJump = tempJump;
}

template <class T>
inline
void
goSignal3DBase<T>::swapXY()
{
    goPtrdiff_t* tempDiff = xDiff;
    goPtrdiff_t* tempJump = myXJump;
    goSize_t     tempSize = mySize.x;

    xDiff = yDiff;
    yDiff = tempDiff;
    myXJump = myYJump;
    myYJump = tempJump;
    mySize.x = mySize.y;
    mySize.y = tempSize;
}

template<class T>
inline
T*
goSignal3DBase<T>::getPtr (goIndex_t x, goIndex_t y, goIndex_t z)
{
    return ptr + myZJump[z] + myYJump[y] + myXJump[x];
}

template<class T>
inline
const T*
goSignal3DBase<T>::getPtr (goIndex_t x, goIndex_t y, goIndex_t z) const
{
    return (const T*) (ptr + myZJump[z] + myYJump[y] + myXJump[x]);
}

template<class T>
inline
T
goSignal3DBase<T>::getClosest (go3Vector<goFloat>& point)
{
    return (*getPtr ((int)point.x, (int)point.y, (int)point.z));
}

inline
goFloat
goSignal3DBase<void*>::sample(go3Vector<goFloat>& point)
{
	return 0.0f;
}

template<class T>
inline
goFloat
goSignal3DBase<T>::sample (go3Vector<goFloat>& point)
{
    int left = (int)point.x;
    goFloat px = point.x - left;
    int top  = (int)point.y;
    goFloat py = point.y - top;
    int front = (int)point.z;
    goFloat pz = point.z - front;

#if 0
	if ( (left < -1) || (left > getSizeX() - 1) || 
		 (top < -1) || (top > getSizeY() - 1) ||
		 (front < -1) || (front > getSizeZ() - 1) )
	{
		cout << "################### \n";
		cout << "\tleft = " << left << ", top = " << top << ", front = " << front << endl;
		return 0;
	}
#endif

    T* p = getPtr (left,top,front);
    T A = *p;
    T B = *(p + xDiff[left]);
    T C = *(p + yDiff[top]); // *getPtr (left,top + 1,front));
    T D = *(p + xDiff[left] + yDiff[top]); // *getPtr (left + 1,top + 1,front));

    p += zDiff[front];
    T E = *p;
    T F = *(p + xDiff[left]);
    T G = *(p + yDiff[top]);
    T H = *(p + xDiff[left] + yDiff[top]);

    goFloat I1;
    SIGNAL3D_bilinear (A,B,C,D,px,py,I1);
    
    goFloat I2;
    SIGNAL3D_bilinear (E,F,G,H,px,py,I2);
    
    return (I1 + (I2 - I1) * pz);
}
#endif

