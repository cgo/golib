#ifndef GOBLOCKSTORE_H
#define GOBLOCKSTORE_H

#include <config.h>
#include <gotypes.h>
#ifdef BLOCKSTORE_USE_HASHTABLE
#include <gohashtable.h>
#endif
#ifdef BLOCKSTORE_USE_HASHCACHE
#include <gohashcache.h>
#endif
#include <gosignal3d.h>
#include <goblockprovider.h>
#include <gotimerobject.h>
#include <goarray.h>
#include <gospecialarrays.h>

class goMutex;

namespace Vol {

template <class T>
class goPresenceManager;

/*!
 * Provides a structure for 
 * <strong>efficiently</strong> accessing blocks which need not be in linear order.
 * @author Christian Gosch
 * @date 7.8.2001
 * @todo Implementation using a hash table works.
 * Add size dependent hash mod value, guess the size in the beginning?
 * Hashtables are not necessarily the best solution for data access.
 * Think about alternatives.
 * Limited error checking is done using goError.
 * The hashtable is very unsafe when used in an MT environment. BLOCKSTORE_USE_ARRAY
 * defines the use of a linear array => fast, large, easier to handle with replacement strategies
 * If needed, add exception handling.
 */
template<class T>
class
  goBlockStore : public goBlockProvider<T>, public goTimerObject, 
  				 public goObjectInfo
{
 public:
  goBlockStore();

  /*!
   * Write destructor.
   */
  virtual ~goBlockStore();

  /*!
   * Set basic size of a goSignal3D representing a block.
   * This must be set before initialization.
   * @param x Size in x direction
   * @param y Size in y direction
   * @param z Size in z direction
   */
  void setBlockSize (goSize_t x, goSize_t y, goSize_t z);

  /*!
   * Sets the maximal number of blocks that can be present.
   */
  void setMaxBlocks (goSize_t m) { maxBlocks = m; }
  
  /*!
   * @see setBlockSize (goSize_t x, goSize_t y, goSize_t z);
   */
  void setBlockSize (const goSize3D& sz);
  void setBlockSize (goSize3D& sz);
  void setPresenceManager (goPresenceManager<T>* p) { pm = p; }
  /*!
   * @throw goExceptionString
   */
  bool init();
  /*!
   * Creates a block and adds it to the internal data structure.
   * The block is thereafter accessible through the given blocknumber.
   * @param blockNumber Access number for the created block.
   * @param sizeFactor Right-shift the basic block size (setBlockSize()) 
   * sizeFactor times and create the block using the new size.
   * Note that the <strong>basic</strong> block size remains the same,
   * the new block size only holds for the new block.
   */
  goSignal3D<T>* createBlock (goSize_t blockNumber, int sizeFactor = 0);

  void setMaxMemoryUsage (goSize_t m) { max_memory_usage = m; }
  void checkMemoryUsage ();

  /*! 
   * Creates a larger block and copies the old data to the low pass position in the block.
   * That enables one to just load higher detail bands and redo a DWT.
   */ 
  goSignal3D<T>* prepareBlockResolutionRaise (goSize_t blockNumber, int resolutionDiff);


  /*!
   * Tries to remove a number of blocks, if not all are tagged as 
   * "too new" by the caching mechanism. Check the size of the object
   * with memoryUsage().
   * That is, however, NOT guaranteed.
   * @return True if blocks have been removed, false if no blocks were available for removal.
   * @see memoryUsage()
   */
  bool removeSomeBlocks ();

  /*!
   * Tries to remove a single block. If the block is still in use (refcount > 0), 
   * the block is unchanged.
   * The block MUST exist. There is no presence check. Called from goPresenceManager::makePresent()
   * @return True if the block could be removed, false otherwise (i.e. block still in use)
   */
  bool removeSingleBlock (goSize_t blockIndex);

  /*!
   * Returns a pointer to a block. Only use for READ ACCESS unless you EXACTLY know what you are
   * doing or you are working in single threaded mode.
   * Other threads might be using the same data at the same time. There is no mutual exclusion, only
   * a protection through a reference counter. The block has to be released by releaseBlock() when it is
   * no longer used by the caller.
   * @return Pointer to the requested block or NULL. If fail() returns true after a call to
   * getBlock(), getBlock() was unsuccessful.
   */
  goSignal3D<T>* 	getBlock (goSize_t blockNumber);
  void				releaseBlock (goSize_t blockNumber);
  /*!
   * Some operations set an internal flag if they fail. This method gives a clue about failure 
   * after such an operation.
   * @return True if the last operation was unsuccessful, false otherwise.
   */
  bool				fail () { return lastFailed; }
  goSize3D& 		getBlockSize();

  /*!
   * From the goObjectInfo base. 
   * @return The approximate memory usage of the object.
   */
  goSize_t			memoryUsage ();

  void _dumpBlockInfo (goSize_t blockNumber)
  {
	  cout << getClassName() << " debugging output for block " << blockNumber << ":" << endl;
	  cout << "\trefCount = " << (int)pm->refCountCheck(blockNumber) << endl;
	  pm->refCountRelease();
	  cout << "\tpresence = " << pm->isPresent (blockNumber) << endl;
	  cout << "\tflags    = " << (int)pm->getFlag (blockNumber) << endl;
	  cout << "\tsize     = " << ((goSignal3D<T>*)blockAddr[blockNumber])->getSizeX() << "," << 
	  	((goSignal3D<T>*)blockAddr[blockNumber])->getSizeY() << "," << ((goSignal3D<T>*)blockAddr[blockNumber])->getSizeZ() << endl;


  }


 private:
  // This is not thread safe. Use it only from within the removeSomeBlocks() and
  // removeSingleBlock() methods.
  void removeBlock (goSize_t blockNumber);
  /*!
   * Block numbers mapped to goSignal3D<T> addresses.
   */
#ifdef BLOCKSTORE_USE_HASHTABLE
  goHashTable<goSize_t, void*> blockAddr;
#else
  #ifdef BLOCKSTORE_USE_HASHCACHE
  goHashCache<goSize_t, void*, T> blockAddr;
  #else
    #ifdef BLOCKSTORE_USE_ARRAY
	goArray<void*> blockAddr;
    #endif
  #endif
#endif  		
  
  goMutex 				blockMutex;  // mutex protecting read/write accesses to blockAddr
  goMutex				checkMemoryMutex;  // used only by checkMemoryUsage() to protect from reentrance
  
  bool			       	lastFailed;  // fail flag -- not used by all the members
  goPresenceManager<T>*	       pm;  //

  goSize3D blockSize;				// Basic size of a block.
  goSize_t maxBlocks;				// Max. number of blocks

  goMutex  mem_mutex;				// mutex to protect total_memory_usage;
  goSize_t total_memory_usage;		// memory in bytes
  goSize_t max_memory_usage;		// max. memory in bytes (setMaxMemoryUsage())
};

};

#endif
