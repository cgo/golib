#ifndef GOCLUSTER3D_H
#define GOCLUSTER3D_H

#include <gotypes.h>
#include <gosignal3d.h>
#include <goarray.h>

namespace Vol {

/*!
 * 3D signal representation as a cluster of blocks of a given size.
 * Features widely untested.
 * @author Christian Gosch
 * @date 18.7.2001
 * @see goSignal3D
 * @see goVolumeFile
 */
template<class T>
class
goCluster3D
{
 public:
  goCluster3D();
  virtual ~goCluster3D();

  /* Size of the 3D signal in samples */
  void setSize (goSize_t sx, goSize_t sy, goSize_t sz);
  /* Size of each block in samples */
  void setBlockSize (goSize_t sx, goSize_t sy, goSize_t sz);
  
  void read (const char* filename);
  void write (const char* filename);

  /* Number of blocks in total */
  goSize_t		getNumberOfBlocks ();
  
  /* Call update after you changed size and blocksize */
  void			update();
  /* Return ptr to block n */
  inline goSignal3D<T>*	getBlock (goIndex_t n);
  
  /* Add block: allocate new goSignal3D and append it to the blocks array */
  void			addBlock (goSignal3D<T>& b);
  /* Add block: append the pointer b to the blocks array */
  void			addBlockPtr (goSignal3D<T>* b);
  
  inline goSize_t	getXBlocks () { return xBlocks; }
  inline goSize_t	getYBlocks () { return yBlocks; }
  inline goSize_t	getZBlocks () { return zBlocks; }
  
  inline goIndex_t	getBlockNr (goIndex_t voxelX, goIndex_t voxelY, goIndex_t voxelZ);
  
 private:
  /* goSignal3D<T> ptr */
  goArray<void*>	blocks;
  goSize_t		xSize, ySize, zSize;
  goSize_t		xBlockSize, yBlockSize, zBlockSize;
  goSize_t		xBlockSizeLog, yBlockSizeLog, zBlockSizeLog;
  goSize_t		xBlocks, yBlocks, zBlocks;
};

template<class T>
inline
goSignal3D<T>*
goCluster3D<T>::getBlock (goIndex_t n)
{
  return (goSignal3D<T>*)blocks[n];
}

template<class T>
inline
goIndex_t
goCluster3D<T>::getBlockNr (goIndex_t voxelX, goIndex_t voxelY, goIndex_t voxelZ)
{
  return (voxelX >> xBlockSizeLog) + \
    (voxelY >> yBlockSizeLog) * xBlocks + \
    (voxelZ >> zBlockSizeLog) * xBlocks * yBlocks;
}

};

#endif
