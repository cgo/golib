#ifndef GORESOLUTIONMANAGER_H
#define GORESOLUTIONMANAGER_H

#include <gotypes.h>
#include <goarray.h>
#include <gospecialarrays.h>
#include <gopresencemanager.h>

namespace Vol {

/*!
 * @author Christian Gosch
 * @date 6.8.2001
 * @see goPresenceManager
 */
class
goResolutionManager
{
 public:
  goResolutionManager();
  virtual ~goResolutionManager();

  void init ();
  inline void setNumberOfBlocks (goSize_t b);
  inline void setMinResolution (goInt8 minRes) { minResolution = minRes; }
  inline int  getMinResolution () { return minResolution; }
  inline int  getResolution (goSize_t blockNumber);

  /*
   * This part about C++ sucks. 
   * Declare goPresenceManager as a friend to enable it to access the setResolution() method (and no other external class).
   * Since each specialization of goPresenceManager is treated as a different type of object, we have to declare
   * each specialization as a friend class. Now does this suck or not?
   */
  friend class goPresenceManager<goInt8>;
  friend class goPresenceManager<goUInt8>;
  friend class goPresenceManager<goInt16>;
  friend class goPresenceManager<goUInt16>;
  friend class goPresenceManager<goInt32>;
  friend class goPresenceManager<goUInt32>;
  friend class goPresenceManager<goFloat>;
  friend class goPresenceManager<goDouble>;

 protected:
  inline void setResolution (goSize_t blockNumber, goInt8 res);

 private:
  /*
   * Containing number of stages loaded for each block
   */
  goNibbleArray *stages;
  goSize_t numberOfBlocks;
  goInt8	minResolution;		// Set by the presence manager when initializing the resolution manager
};

inline void goResolutionManager::setNumberOfBlocks (goSize_t b)
{
  numberOfBlocks = b;
}

inline int goResolutionManager::getResolution (goSize_t blockNumber)
{
  return (int)(*stages)[blockNumber];
}

inline 
void 
goResolutionManager::setResolution (goSize_t blockNumber, goInt8 res)
{
  stages->set(blockNumber, res);
}

};

#endif
