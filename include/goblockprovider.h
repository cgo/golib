#ifndef GOBLOCKPROVIDER_H
#define GOBLOCKPROVIDER_H

#include <gotypes.h>
#include <gosignal3d.h>

namespace Vol {

/*!
 * Interface for classes providing block access.
 * @author Christian Gosch
 * @date 11.8.2001
 * @addtogroup DA
 */
template<class T>
class
goBlockProvider
{
 public:
  goBlockProvider();
  virtual ~goBlockProvider();

  /*! 
   * Gets a pointer to a block.
   * @param blockIndex Identifier for the block
   * @return Pointer to the requested block or NULL.
   */
  virtual goSignal3D<T>* getBlock (goSize_t blockIndex);
  /*!
   * Release a block if it is not needed by the caller anymore. getBlock MUST have been called beforehand.
   * A derived class may want to provide some kind of protection to each block like a reference counter.
   * @param blockIndex Identifier for the block to be released.
   * @see goBlockStore
   */
  virtual void releaseBlock (goSize_t blockIndex);
  virtual int getStages ();
  virtual goSize3D& getBlockSize ();

 private:
  goSize3D dummy;
};

}

#endif
