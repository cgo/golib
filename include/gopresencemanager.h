/* --*-C++-*-- */

#ifndef GOPRESENCEMANAGER_H
#define GOPRESENCEMANAGER_H

#include <gotypes.h>
#include <goviewvolume.h>
#include <goarray.h>
#include <gospecialarrays.h>
#include <govolumefile.h>
#include <goblockstore.h>
#include <gocache.h>
#include <gotimerobject.h>
#include <gothread.h>

namespace Vol {

class goResolutionManager;

	/// Flags for goPresenceManager::setFlag() to indicate level of protection from deletion
	enum PM_FLAGS {
		PM_NO_FLAGS 		= 0,    		// |
		PM_PROTECTED 		= 3,    		// |
		PM_MAX_PROTECTION	= PM_PROTECTED, // |   There are the protection levels.
		PM_LESS_PROTECTED 	= 2,    		//  >  These can NOT be OR'ed together
		PM_LESSER_PROTECTED = 1,    		// |
		PM_PREDICTED 		= 8,	// this can be or'ed to another value
		PM_SCHEDULED		= 16,   // indicates whether a block is already scheduled for loading
		PM_FLAGS_MASK		= 24    // Mask to AND out the actual flags from the protection level
	};


/*!
 * Manages the presence and absence of blocks.
 * Each block is represented by one bit in a bit field. If the
 * bit is set, the block is present and no further action needs to be
 * taken. If it is not set, the block is known not to reside in memory
 * and must be loaded from a secondary medium, like a disk.
 * The appropriate action will be taken.
 * @author Christian Gosch
 * @date 5.8.2001
 * @see goResolutionManager
 * @see goViewManager
 */
template <class T>
class
goPresenceManager :  public goTimerObject
{
 public:
    /*!
     * @todo Make the cache size auto-adapt to the main memory size
     * @see goCache::cacheSetMaxSize()
     */
  goPresenceManager ();
  ///
  virtual ~goPresenceManager();

  /*!
   * @throw goExceptionString
   * @return True on success, false otherwise.
   */
  bool init ();

	
	inline void lock () { pmMutex.lock(); }
	inline void unlock () { pmMutex.unlock(); }
    /*!
     * @param blockNumber Index number of a block
     * @return True if the block is present, false otherwise.
     */ 
  bool isPresent (goSize_t blockNumber);
    /*!
	 * @attention This method is not up to date, since it is not used
	 * in the current system!
     * Makes a block memory resident (triggers loading from a secondary 
     * storage medium).
     * @param blockNumber Index number of the block to be made memory resident
     * @param resolution Resolution (stage) up to which the block 
     * is to be loaded
     */
  void makePresent (goSize_t blockNumber, goInt8 resolution);
    /*!
     * Makes some blocks memory resident (triggers loading from a secondary 
     * storage medium).
     * @param block_numbers Indices of the blocks to be made memory resident
     * @param __resolutions Resolutions (stage) up to which each block 
     * is to be loaded
	 * @param waitUntilLoaded If true, waits for the loading thread until all blocks are loaded. This is the default.
     */
  void makePresent (goArray<goSize_t>& block_numbers, goArray<int>& __resolutions, bool waitUntilLoaded = true);

  /// Don't use, it does not really remove anything at the moment.
  // void remove (goSize_t blockNumber);

    /*!
     * Sets the resolution manager to be used to check for block resolutions.
     * @param m Pointer to a valid goResolutionManager
     */
  void setResolutionManager (goResolutionManager* m);
    /*!
     * Sets the volume file that provides an interface to
     * secondary storage media containing the volume data.
     * @param f Pointer to a valid goVolumeFile
     */
  void setVolumeFile (goVolumeFile<T> *f);
    /*!
     * Sets the block store to hold any loaded block data in memory.
     * @param s Pointer to a valid goBlockStore
     */
    void setBlockStore (goBlockStore<T> *s);

    /*!
     * Sets the presence bit to one.
     * Used by goVolumeFile to indicate a block has been loaded.
     * @attention Do not use this method unless you know EXACTLY what it means!
     * Do not blame anyone except yourself for any damage!
     * @param blockNumber Number of the block for which the presence bit is set to one.
     */ 
    inline void set (goSize_t blockNumber) 
	{ 
		lock(); 
		presence->set (blockNumber);			// Set presence
		rmFlags[blockNumber] &= ~PM_SCHEDULED;	// clear SCHEDULED
		unlock(); 
	}
	
    inline void unSet (goSize_t blockNumber) 
	{ 
		lock(); 
		presence->unSet (blockNumber); 
		unlock(); 
	}

	
	/*!
	 * Sets the protection level flag for a block.
	 * Does not preserve old flag values, but deletes them and sets the new value.
	 * If you want to keep old flags, you have to or them together yourself.
	 * @param flag Flag value
	 */
	inline void setFlag (PM_FLAGS flag, goSize_t blockIndex);
	inline void unSetFlag (PM_FLAGS flag, goSize_t blockIndex);
	inline void setProtection (PM_FLAGS p, goSize_t blockIndex);

	inline goUInt8 getFlag (goSize_t blockIndex);
	/// Tests only the enumerated flags, NOT the PM_PREDICTED attribute
	inline bool testProtection (goUInt8 flag, PM_FLAGS test);
	/// Locks the object, Sets presence bit = 0, and sets SCHEDULED flag
	inline void setScheduled 	 (goSize_t blockIndex);
	/// Locks the object and clears SCHEDULED flag
	inline void unSetScheduled 	 (goSize_t blockIndex);
	inline bool isScheduled 	 (goSize_t blockIndex);
	/*
	 * Tries to do the following atomically:
	 * 	if refCount for block is 0 AND block is not already PM_SCHEDULED:
	 *  	leave presence bit untouched
	 *		set PM_SCHEDULED flag
	 * @return True if block could be scheduled, false otherwise (refCount is > 0, 
	 * i.e. the block
	 * is being in use or PM_SCHEDULED is set, i.e. block was already scheduled)
	 */
	inline bool tryScheduleBlock (goSize_t blockIndex);
	

	/// Locks the object, returns refcount for blockIndex. Unlock with refCountRelease()!
	inline goUInt8 refCountCheck(goSize_t blockIndex);
	/// Unlocks the object
	inline void	   refCountRelease();
	/*! 
	 * Locks the object, returns true if removal of block is permitted, false otherwise.
	 * Checks presence == 1 and refCount == 0, nothing else. Flags are not checked. 
	 */
	inline bool    removalCheck(goSize_t blockIndex);
	/// Unlocks the object
	inline void	   removalRelease();
	/// Sets presence = 0 without locking. Use within removalCheck() - removalRelease() 
	inline void	   removalRemoved(goSize_t blockIndex);

	inline bool refCountInc (goSize_t blockNumber);
	inline bool refCountDec (goSize_t blockNumber);

 private:

  	goArray<goUInt8> refCount;	  // reference counts indicating if a block is currently in use
  								  // i.e. was accessed by blockstore->getBlock(). releaseBlock() decrements
								  // an entry in this array. The array MUST BE PROTECTED by a mutex.
  	// goMutex				refCountMutex;									  
	
  	goBitArray 		*presence;
	
	// goNibbleArray   *rmFlags;    // 4 flags for each block indicating how "removable" the block is. Set by view manager
	goArray<goUInt8>   rmFlags;    // 4 flags for each block indicating how "removable" the block is. Set by view manager
	
  	goMutex	 pmMutex;
  	goResolutionManager *rm;

	goVolumeFile<T> *file;
  	goBlockStore<T> *store;

	goMutex			arrayMutex;		// mutex to protect the below arrays
  	goArray<void*>	signalPointers;	// Helper array for multi threaded block loading
  	goArray<goSize_t>	blockNumbers;	// Helper array for multi threaded block loading
  	goArray<int>      resolutions;	// Helper array for multi threaded block loading
  	goArray<int>		resolutionsMin; // Lowest resolutions to be read (helper for loading selected bands)

								// and used by block store to determine which blocks to delete if deletion is
								// necessary.
};

template <class T>
inline
bool
goPresenceManager<T>::removalCheck(goSize_t blockIndex)
{
	lock();
	if ( ((*presence)[blockIndex] == 1) &&
		 (refCount[blockIndex] == 0) )  // FIXME
	{
		// FIXME
		return true;
	}
	return false;
}

template <class T>
inline
void
goPresenceManager<T>::removalRelease()
{
	unlock();
}

template <class T>
inline
void
goPresenceManager<T>::removalRemoved(goSize_t blockIndex)
{
	presence->unSet(blockIndex);
}

template <class T>
inline
bool
goPresenceManager<T>::tryScheduleBlock (goSize_t blockIndex)
{
	bool retval = false;
	lock();
	goUInt8 rc = refCount[blockIndex];
	// test if refcount > 0 or block is already scheduled
	if ((rc > 0) || ((rmFlags[blockIndex] & PM_SCHEDULED) != 0))
	{
		retval = false;
	} else
	{
		// Set scheduled flag 
		rmFlags[blockIndex] |= PM_SCHEDULED;
		// presence->unSet(blockIndex); // leads to a deadlock because this is used
		// while scheduling blocks in goPresenceManager.
		retval = true;
	}
	unlock();
	return retval;
}

template <class T>
inline
bool
goPresenceManager<T>::testProtection(goUInt8 flag, PM_FLAGS test)
{
	return ( (flag & (~((goUInt8)PM_FLAGS_MASK))) == (goUInt8)test );
}

template <class T>
inline
void
goPresenceManager<T>::setScheduled (goSize_t blockIndex)
{
	lock();
	presence->unSet (blockIndex);
	rmFlags[blockIndex] |= PM_SCHEDULED;
	unlock();
}

template <class T>
inline
void
goPresenceManager<T>::unSetScheduled (goSize_t blockIndex)
{
	lock();
	rmFlags[blockIndex] &= ~PM_SCHEDULED;
	unlock();
}

template <class T>
inline
bool
goPresenceManager<T>::isScheduled (goSize_t blockIndex)
{
	bool retval;
//	lock();  // would result in a deadlock when used in blockstore
	retval = (rmFlags[blockIndex] & PM_SCHEDULED) == PM_SCHEDULED;
//	unlock();
	return retval;
}

// Accepts PM_NOFLAGS ... PM_PROTECTED
template <class T>
inline
void
goPresenceManager<T>::setProtection (PM_FLAGS p, goSize_t blockIndex)
{
	lock();
	goUInt8 f = rmFlags[blockIndex];
	// Set the protection level, preserve the upper bits flags
#if _GODEBUG >= 4	
	cout << getClassName() << "::setProtection(): " << (int)f << " --> " << (int)((f & (PM_FLAGS_MASK)) | (goUInt8)p) << "\n";
	cout << getClassName() << ": Setting block " << blockIndex << " to protection level & flags " << (int)f << endl;
#endif	
	f = (f & PM_FLAGS_MASK) | (goUInt8)p;
	rmFlags[blockIndex] = f;
	unlock();
}

// Accepts the upper bit flags PM_PREDICTED and PM_SCHEDULED
template<class T>
inline
void
goPresenceManager<T>::setFlag (PM_FLAGS flag, goSize_t blockIndex)
{
	// rmFlags->OR (flag, blockIndex); 
	// Set the flags nibble HARD, i.e. old flag values get lost.
	if (flag <= PM_MAX_PROTECTION)
	 {
		 cout << getClassName() << ": Caller tried to set flag in protection levels!" << endl;
		 cout << "\tFlag NOT set." << endl;
		 return;
	 }
#if _GODEBUG >= 3
	cout << getClassName() << ": tagging block " << blockIndex << " as ";
	if ( (flag & PM_PREDICTED) == PM_PREDICTED )
	{
		cout << " PM_PREDICTED";
	}
	if ( (flag & PM_SCHEDULED) == PM_SCHEDULED )
	{
		cout << " PM_SCHEDULED";
	}
	cout << endl;
#endif
	// rmFlags->set(blockIndex,(goInt8)flag);
	// Set the flags, preserve the protection level
	lock();
	rmFlags[blockIndex] |= (goUInt8)flag;
	unlock();
}

template<class T>
inline
void
goPresenceManager<T>::unSetFlag (PM_FLAGS flag, goSize_t blockIndex)
{
	lock();
	rmFlags[blockIndex] &= ~((goUInt8)flag);
	unlock();
}

template <class T>
inline
goUInt8
goPresenceManager<T>::getFlag(goSize_t blockIndex)
{
	// return (enum Vol::PM_FLAGS)(*rmFlags)[blockIndex];
	goUInt8 retval;
//	lock();
	retval = (rmFlags[blockIndex]);
//	unlock();
	return retval;
}


template <class T>
inline
goUInt8
goPresenceManager<T>::refCountCheck (goSize_t blockIndex)
{
	lock();
	return refCount[blockIndex];
}

template <class T>
inline
void
goPresenceManager<T>::refCountRelease ()
{
	unlock();
}

template <class T>
inline
bool
goPresenceManager<T>::refCountInc (goSize_t blockNumber)
{
	lock();
	if (refCount[blockNumber] == 255)
	{
		unlock();
		return false;
	}
	refCount[blockNumber]++;	
	unlock();
	return true;
}

template <class T>
inline
bool
goPresenceManager<T>::refCountDec  (goSize_t blockNumber)
{
	lock();
	if (refCount[blockNumber] == 0)
	{
		unlock();
		return false;
	}
	refCount[blockNumber]--;	
	unlock();
	return true;
}

};

#endif
