#ifndef __GOSIGNAL3D_H__
#define __GOSIGNAL3D_H__

#include <config.h>
#include <iostream.h>
#include <fstream.h>
#include <gotypes.h>
#include <goobjectinfo.h>
#include <go3vector.h>		// sample()

#include <string.h>

#ifdef GO_DIPLOMARBEIT
#include <gotransferfunction.h>
#include <govol.h>
#endif

/*!
 * Class to handle 3-dimensional signals.
 * Assumes a 3D signal is stored linearly in memory.
 * xDiff, yDiff, and zDiff, are the pointer differences in each 
 * direction.
 * ptr points at the start address of the signal.
 */
template< class T >
class
goSignal3D : public goObjectInfo {
 public:
  enum Neighbour 
   {
     LEFT, RIGHT, TOP, BOTTOM, FRONT, BACK, EDGE1, EDGE2, EDGE3, EDGE4, EDGE5,
			 EDGE6, EDGE7, EDGE8, EDGE9, EDGE10, EDGE11, EDGE12,
			 CORNER1, CORNER2, CORNER3, CORNER4, CORNER5, CORNER6, CORNER7, CORNER8 
     };
		 
  goSignal3D ();
  goSignal3D (goSize_t x, goSize_t y, goSize_t z,
	      goSize_t border_x = 0, goSize_t border_y = 0, goSize_t border_z = 0);
  goSignal3D (goSignal3D &other);
  virtual ~goSignal3D ();

  // From goObjectInfo
  virtual goSize_t memoryUsage();

  /*!
   * Allocates memory of appropriate size for the block.
   * Sets diffs and size.
   * The data is uninitialized.
   * If make is called, <CODE>destroy()</CODE> should be called when
   * the data is not referenced anymore.
   * destroy() is NOT called automagically when the destructor is called.
   * To destroy the actual data, destroy() needs to be called BEFORE deleting the object.
   * @param x Size in x direction of the signal
   * @param y Size in y direction of the signal
   * @param z Size in z direction of the signal
   * @param border_x Size of the border in x direction
   * @param border_y Size of the border in y direction
   * @param border_z Size of the border in z direction
   */
  inline void make (goSize_t x, goSize_t y, goSize_t z,
		    goSize_t border_x = 0, goSize_t border_y = 0, goSize_t border_z = 0);
  /// Copies only the size, NOT THE DATA!
  void make (goSignal3D *other);
  /*!
   * Deletes the memory used by the block data.
   * @todo Take care what happens when axes are rotated. Nothing bad should happen though.
   */
  inline void destroy () 
    {
      delete[] real_ptr;
	  real_ptr = 0;
	  ptr = 0;	  
    }

  /*!
   * Reads block data from file stream <CODE>f</CODE>.
   */
  bool read (ifstream &f, bool no_extra_memory = false);
  bool readSlice (ifstream &f, goIndex_t slice, bool no_extra_memory = false);

  /*!
   * Writes this signal into ofstream f, slicewise using writeSlice().
   */
  bool write (ofstream &f, bool no_extra_memory = false);
  /*!
   * Writes slice number <code>slice</code> to ofstream f.
   */
  bool writeSlice (ofstream &f, goIndex_t slice, bool no_extra_memory = false);

  /*!
   * Sets the internal pointer to the block data.
   * If this is set, size and pointer difference values must be
   * set manually as well.
   */
  void setPtr (T *p); 

  /*!
   * \return The pointer to the linearly organised data.
   */
  inline T* getPtr () { return ptr;}

  /*!
   * @return The real pointer. Don't use this unless you know EXACTLY what you are
   * doing.
   */
  inline T* getRealPtr () { return real_ptr; }
  
  /*!
   * \return The pointer to the data at position (x,y,z).
   */
  inline T* getPtr (goInt32 x, goInt32 y, goInt32 z)
    { return ( ptr + (z * zDiff + y * yDiff + x * xDiff) ); }
  /*!
   * Sets the pointer differences in x,y, and z directions.
   * A pointer difference gives the memory "distance" between two
   * points, lines, or planes.
   */
  inline void setDiff (goPtrdiff_t x, goPtrdiff_t y, goPtrdiff_t z)
    { xDiff = x; yDiff = y; zDiff = z; }
  /*!
   * \see <code>setDiff()</code>
   */
  inline void setXDiff (goPtrdiff_t d) { xDiff = d;}
  /*!
   * \see <code>setDiff()</code>
   */
  inline void setYDiff (goPtrdiff_t d) { yDiff = d;}
  /*!
   * \see <code>setDiff()</code>
   */
  inline void setZDiff (goPtrdiff_t d) { zDiff = d;}
  /*!
   * \see <code>setDiff()</code>
   */
  inline goPtrdiff_t getXDiff () { return xDiff; }
  /*!
   * \see <code>setDiff()</code>
   */
  inline goPtrdiff_t getYDiff () { return yDiff; }
  /*!
   * \see <code>setDiff()</code>
   */
  inline goPtrdiff_t getZDiff () { return zDiff; }
  /*!
   * \see <code>setDiff()</code>
   */
  inline const goPtrdiff_t getXDiff () const { return xDiff; }
  /*!
   * \see <code>setDiff()</code>
   */
  inline const goPtrdiff_t getYDiff () const { return yDiff; }
  /*!
   * \see <code>setDiff()</code>
   */
  inline const goPtrdiff_t getZDiff () const { return zDiff; }

  /*!
   * Sets the size of the signal in samples in x, y, and z directions.
   */
  inline void setSize (goSize_t x,goSize_t y,goSize_t z)
    { xSize = x; ySize = y; zSize = z; }
  inline void setSizeX(goSize_t s) { xSize = s; }
  inline void setSizeY(goSize_t s) { ySize = s; }
  inline void setSizeZ(goSize_t s) { zSize = s; }
  /*!
   * \return Size in samples in x direction.
   */
  inline const goSize_t getSizeX () const { return xSize; }
  /*!
   * \return Size in samples in y direction.
   */
  inline const goSize_t getSizeY () const { return ySize; }
  /*!
   * \return Size in samples in z direction.
   */
  inline const goSize_t getSizeZ () const { return zSize; }

  inline const goSize_t getBorderX () { return borderX; }
  inline const goSize_t getBorderY () { return borderY; }
  inline const goSize_t getBorderZ () { return borderZ; }
  /*!
   * Does <strong>not</strong> perform a deep copy, instead copies size and pointer difference
   * values and the <strong>pointer</strong> to the signal data.
   */
  goSignal3D&	operator= (goSignal3D &other);
  /*!
   * "Deep" comparison of the actual data.
   */ 
  bool		operator== (goSignal3D &other);

  /*!
   * @return The size of the object data (without the object overhead) in bytes.
   */
  goSize_t	getSize ();

  T	getMaximum();
  T	getMinimum();
  void	fill (T value);
  /// Works only with linear-memory blocks. No sub blocks. Uses memset().
  void  fillByte (goInt8 b) { memset ((void*)real_ptr, (int)b, sizeof(T) * (xSize + (borderX << 1)) * 
  								(ySize + (borderY << 1)) * (zSize + (borderZ << 1)) ); }
  /// Copies the last valid values from the block data into the borders
  void  interpolateBorders ();

  /*! copies a side from the other signal in the border of this signal (border = 1)
   *  If you want to copy all sides and take the edges into account,
   *  take the order LEFT RIGHT TOP BOTTOM FRONT BACK to copy.
   *  See source code for details.
   */
  void  interpolateFromSignal (goSignal3D<T>& other, Neighbour n);
  
  inline void shiftLeftDiff (int n)
    {
      xDiff <<= n;
      yDiff <<= n;
      zDiff <<= n;
    }
  inline void shiftRightDiff (int n)
    {
      xDiff >>= n;
      yDiff >>= n;
      zDiff >>= n;
    }
  inline void shiftLeftSize (int n)
    {
      xSize <<= n;
      ySize <<= n;
      zSize <<= n;
    }
  inline void shiftRightSize (int n)
    {
      xSize >>= n;
      ySize >>= n;
      zSize >>= n;
    }

  /*!
   * Not threadsafe
   */
  inline void rotateAxes ();

  inline T	  getClosest (go3Vector<goFloat>& point);
  inline goFloat sample (go3Vector<goFloat>& point);
#ifdef GO_DIPLOMARBEIT
  inline volFloat sample (go3Vector<goFloat>& point, goTransferFunction<T,volFloat>& tf);
#endif    

 protected:
  /* pointer to the first voxel */
  T		*ptr;
  /* pointer to the first allocated data element */
  T		*real_ptr;
  goPtrdiff_t	xDiff, yDiff, zDiff;
  goSize_t	xSize, ySize, zSize;
  goSize_t	borderX, borderY, borderZ;
};

#define SIGNAL3D_bilinear(__A, __B, __C, __D, __px, __py, __target) {  \
    goFloat __p1 = __A + ((__B - __A)*__px);				\
    goFloat __p2 = __C + ((__D - __C)*__px);				\
    __target =  (__p1 + ((__p2 - __p1)*__py));				\
}


template< class T >
inline
void
goSignal3D<T>::make (goSize_t x, goSize_t y, goSize_t z,
		     goSize_t border_x, goSize_t border_y, goSize_t border_z) {
  real_ptr = new T[(x + (border_x << 1)) * (y + (border_y << 1)) * (z + (border_z << 1))];
  xDiff = 1;
  yDiff = x + (border_x << 1);
  zDiff = yDiff * (y + (border_y << 1));
  setSize (x,y,z);
  ptr = real_ptr + (goPtrdiff_t)(border_x * xDiff + border_y * yDiff + border_z * zDiff);
  borderX = border_x;
  borderY = border_y;
  borderZ = border_z;
}

template< class T >
inline
void
goSignal3D<T>::rotateAxes ()
{
  goPtrdiff_t tempDiff = zDiff;
  goSize_t tempSize = zSize;
  
  zSize = ySize;
  ySize = xSize;
  xSize = tempSize;
  zDiff = yDiff;
  yDiff = xDiff;
  xDiff = tempDiff;
}

template<class T>
inline
T
goSignal3D<T>::getClosest (go3Vector<goFloat>& point)
{
    return (*getPtr ((int)point.x, (int)point.y, (int)point.z));
}

inline
goFloat
goSignal3D<void*>::sample(go3Vector<goFloat>& point)
{
	return 0.0f;
}

template<class T>
inline
goFloat
goSignal3D<T>::sample (go3Vector<goFloat>& point)
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
    T B = *(p + xDiff);
    T C = *(p + yDiff); // *getPtr (left,top + 1,front));
    T D = *(p + xDiff + yDiff); // *getPtr (left + 1,top + 1,front));

    p += zDiff;
    T E = *p;
    T F = *(p + xDiff);
    T G = *(p + yDiff);
    T H = *(p + xDiff + yDiff);

    goFloat I1;
    SIGNAL3D_bilinear (A,B,C,D,px,py,I1);
    
    goFloat I2;
    SIGNAL3D_bilinear (E,F,G,H,px,py,I2);
    
    return (I1 + (I2 - I1) * pz);
}

#ifdef GO_DIPLOMARBEIT


template<class T>
inline 
volFloat 
goSignal3D<T>::sample (go3Vector<goFloat>& point, goTransferFunction<T,volFloat>& tf)
{
    int left = (int)point.x;
    goFloat px = point.x - left;
    int top  = (int)point.y;
    goFloat py = point.y - top;
    int front = (int)point.z;
    goFloat pz = point.z - front;

    T* p = getPtr (left,top,front);
    T A = *p;
    T B = *(p + xDiff);
    T C = *(p + yDiff); // *getPtr (left,top + 1,front));
    T D = *(p + xDiff + yDiff); // *getPtr (left + 1,top + 1,front));

    p += zDiff;
    T E = *p;
    T F = *(p + xDiff);
    T G = *(p + yDiff);
    T H = *(p + xDiff + yDiff);

    volFloat I1;
	volFloat _A,_B,_C,_D;
	_A = tf[A]; _B = tf[B]; _C = tf[C]; _D = tf[D];
    SIGNAL3D_bilinear (_A,_B,_C,_D,px,py,I1);
    
    volFloat I2;
	volFloat _E,_F,_G,_H;
	_E = tf[E]; _F = tf[F]; _G = tf[G]; _H = tf[H];
    SIGNAL3D_bilinear (_E,_F,_G,_H,px,py,I2);
    
    return (I1 + (I2 - I1) * pz);
}
#endif

/*!
 * \example filter3d.cc
 * This is an example and test program for the macros used to filter goSignal3D
 * objects. See the source code for details.
 * @author Christian Gosch
 */

#endif



