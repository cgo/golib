#ifndef __GO3DBLOCK_H__
#define __GO3DBLOCK_H__

#include <iostream.h>
#include <fstream.h>
#include <gotypes.h>
#include <gocacheable.h>


/*!
 * Assumes a volume block is stored linearly in memory.
 * xDiff, yDiff, and zDiff, are the pointer differences in each 
 * direction.
 * ptr points at the start address of the block.
 */
template< class T >
class
go3DBlock : public goCacheable {
 public:
  go3DBlock ();
  go3DBlock (go3DBlock &other);
  virtual ~go3DBlock ();

  /*!
   * Allocates memory of appropriate size for the block.
   * Sets diffs and size.
   * The data is uninitialized.
   * If make is called, <CODE>destroy()</CODE> should be called when
   * the data is not referenced anymore.
   */
  void make (goSize_t x, goSize_t y, goSize_t z);
  /// Copies only the size, NOT THE DATA!
  void make (go3DBlock *other);
  /*!
   * Deletes the memory used by the block data.
   */
  void destroy ();

  /*!
   * Reads block data from file stream <CODE>f</CODE>.
   */
  bool read (ifstream &f);
  bool readSlice (ifstream &f, goIndex_t slice);

  bool write (ofstream &f);
  bool writeSlice (ofstream &f, goIndex_t slice);

  /*!
   * Sets the internal pointer to the block data.
   * If this is set, size and pointer difference values must be
   * set too.
   */
  void setPtr (T *p) { ptr = p; } 
  T* getPtr () { return ptr;}
  T* getPtr (goInt32 x, goInt32 y, goInt32 z)
    { return ( ptr + (z * zDiff + y * yDiff + x * xDiff) ); }
  void setDiff (goPtrdiff_t x, goPtrdiff_t y, goPtrdiff_t z)
    { xDiff = x; yDiff = y; zDiff = z; }
  void setXDiff (goPtrdiff_t d) { xDiff = d;}
  void setYDiff (goPtrdiff_t d) { yDiff = d;}
  void setZDiff (goPtrdiff_t d) { zDiff = d;}
  inline goPtrdiff_t getXDiff () { return xDiff; }
  inline goPtrdiff_t getYDiff () { return yDiff; }
  inline goPtrdiff_t getZDiff () { return zDiff; }
  inline const goPtrdiff_t getXDiff () const { return xDiff; }
  inline const goPtrdiff_t getYDiff () const { return yDiff; }
  inline const goPtrdiff_t getZDiff () const { return zDiff; }

  void setSize (goSize_t x,goSize_t y,goSize_t z)
    { xSize = x; ySize = y; zSize = z; }
  inline const goSize_t getSizeX () { return xSize; }
  inline const goSize_t getSizeY () { return ySize; }
  inline const goSize_t getSizeZ () { return zSize; }

  go3DBlock&	operator= (go3DBlock &other);
  /// "Deep" comparison of the actual data.
  bool		operator== (go3DBlock &other);

  T	getMaximum();
  T	getMinimum();
 protected:
  T		*ptr;
  goPtrdiff_t	xDiff, yDiff, zDiff;
  goSize_t	xSize, ySize, zSize;
};



#endif
