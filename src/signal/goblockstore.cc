#include <goblockstore.h>
#include <gostring.h>
#include <goerror.h>
#include <stdio.h>  //sprintf
#include <gosignalmacros.h>
#include <gopresencemanager.h>
#include <goexception.h>
#include <config.h>
#include <gothread.h>

namespace Vol {

#define GO_BLOCKSTORE_SIGNALBORDER 1		// Border width in elements around each block

template <class T>
goBlockStore<T>::goBlockStore()
{
  setClassName ("goBlockStore");
  blockSize.x = 0;
  blockSize.y = 0;
  blockSize.z = 0;
  lastFailed  = false;
  total_memory_usage = 0;
  max_memory_usage   = 0;
  pm = 0;
#ifdef BLOCKSTORE_USE_ARRAY
  blockAddr.resize(0);
#endif
  maxBlocks = 0;
}

template <class T>
goBlockStore<T>::~goBlockStore()
{
}

template <class T>
void
goBlockStore<T>::setBlockSize (goSize_t x, goSize_t y, goSize_t z)
{
  blockSize.x = x;
  blockSize.y = y;
  blockSize.z = z;
} 

template <class T>
void
goBlockStore<T>::setBlockSize (goSize3D& sz)
{
  blockSize = sz;
}

template <class T>
void
goBlockStore<T>::setBlockSize (const goSize3D& sz)
{
  blockSize.x = sz.x;
  blockSize.y = sz.y;
  blockSize.z = sz.z;
}

template <class T>
bool
goBlockStore<T>::init()
{
  /*
   * Add size dependent hash mod value, guess the size in the beginning?
   */ 
       if (!pm) {
	   goString s;
	   s = "goBlockStore::init() has no presence manager set";
	   throw (goExceptionString(s));
	   return false;
       }
#ifdef BLOCKSTORE_USE_HASHTABLE
    blockAddr.setModValue (511);
#else
 #ifdef BLOCKSTORE_USE_HASHCACHE
    blockAddr.setModValue (1023);
 #else 
  #ifdef BLOCKSTORE_USE_ARRAY
    blockAddr.resize (maxBlocks);	// Use an array: resize to the max. number of blocks.
									// That can be memory consuming depending on the data set size/
									// block size ratio, i.e. depending on maxBlocks
	blockAddr.byteFill (0);									
  #endif
 #endif
#endif  	
		
    return true;
}

template <class T>
goSignal3D<T>*
goBlockStore<T>::createBlock (goSize_t blockNumber, int sizeFactor)
{

	goUInt8 r = pm->refCountCheck(blockNumber);
	if (r > 0)
	{
		goString s;
		s = getClassName();
		s += "::createBlock()";
		goError::note(s.toCharPtr(),"refCount of block that was to be created is greater than zero");
		pm->refCountRelease();
		return 0;
	}

  goSignal3D<T>* b = new goSignal3D<T> (blockSize.x >> sizeFactor,
					blockSize.x >> sizeFactor,
					blockSize.x >> sizeFactor,
					GO_BLOCKSTORE_SIGNALBORDER, 
					GO_BLOCKSTORE_SIGNALBORDER, 
					GO_BLOCKSTORE_SIGNALBORDER);
  mem_mutex.lock();
  total_memory_usage += b->memoryUsage();
  mem_mutex.unlock();
  //goString s;
  //s = "Adding block";
  //goError::note("goBlockStore::createBlock()",s);
#ifdef BLOCKSTORE_USE_HASHCACHE  
  	blockAddr.add (blockNumber, (void*)b);
#else
#ifdef BLOCKSTORE_USE_HASHTABLE
	blockAddr.add (blockNumber, (void*)b);
#else 
#ifdef BLOCKSTORE_USE_ARRAY
#if _GODEBUG >= 2						
    if (blockAddr[blockNumber])
	{
		cout << getClassName() << ": createBlock called even though block " << blockNumber << " was apparently already existing!!!" << endl;
	}
#endif			
			
    blockAddr[blockNumber] = (void*)b;
	if (!b)
	{
		cout << getClassName() << ": createBlock could not create block!" << endl;
		pm->refCountRelease();
		return 0;
	}
	  #endif
	#endif
#endif	  	
	
  b->fillByte (0);
#ifndef BLOCKSTORE_USE_ARRAY
  if (blockAddr.fail())
    {
      char number[256];
      sprintf(&number[0], "%d", blockNumber);
      goString s;
      s = "Could not add block number ";
      s += &number[0];
      goError::print("goBlockStore::createBlock()",s);
	  pm->refCountRelease();
      return 0;
    }
#endif	
	pm->refCountRelease();
  return b;
}

template <class T>
void
goBlockStore<T>::checkMemoryUsage ()
{
#if _GODEBUG >= 3
	cout << "blockStore: checking memory usage" << endl;
#endif	
    goSize_t thresh = max_memory_usage;


	checkMemoryMutex.lock();	
    while (total_memory_usage > thresh)
	{
	    // Just remove up to as many blocks as the cache's mod value. 
	    // Maybe replace that value with a different one, see how it works.
#if _GODEBUG >= 3		
		cout << "blockStore: removing some blocks" << endl;
#endif
		// blockMutex.lock();  // Called from within the routine!
	    if (!removeSomeBlocks ())
			break;
		// blockMutex.unlock();	
	}
	checkMemoryMutex.unlock();
	// hashcache method -- not used anymore
    // blockAddr.incrementTime();
}

template<class T>
goSignal3D<T>*
goBlockStore<T>::prepareBlockResolutionRaise (goSize_t blockNumber, int resolutionDiff)
{
    goSignal3D<T>* b = 0, *newB = 0;
	// blockMutex.lock();

	
	goUInt8 r = pm->refCountCheck(blockNumber);	// locks pm
						     // prevent a call to remove*Block[s] to delete b while we work with it
							 // That should usually not happen because PM_PROTECTED should be
							 // set for b by the viewmanager, but it can potentially happen
							 // since single blocks can be removed checking only
							 // for the refcount.
	
#ifdef BLOCKSTORE_USE_HASHCACHE	
    b = (goSignal3D<T>*)blockAddr[blockNumber];		// Get the old block pointer
#else
	#ifdef BLOCKSTORE_USE_HASHTABLE
    b = (goSignal3D<T>*)blockAddr[blockNumber];		// Get the old block pointer
    #else
		#ifdef BLOCKSTORE_USE_ARRAY
		b = (goSignal3D<T>*)blockAddr[blockNumber];
		#endif
	#endif
#endif

	if (!b)
	{
		goError::print("goBlockStore::prepareBlockResolutionRaise()","Fatal: Block to be raised is NULL pointer");
		pm->refCountRelease();
		return 0;
	}

	if (r == 0)
	{
    	newB = new goSignal3D<T> (b->getSizeX() << resolutionDiff, 
		      b->getSizeY() << resolutionDiff, 
		      b->getSizeZ() << resolutionDiff,
		      GO_BLOCKSTORE_SIGNALBORDER, 
		      GO_BLOCKSTORE_SIGNALBORDER, 
		      GO_BLOCKSTORE_SIGNALBORDER);
		// Das ist suboptimal. Eigentlich müssten nur die Ränder genullt werden.
		// Aber um der Zeit willen.... :-((
		newB->fillByte(0);
		newB->shiftLeftDiff (resolutionDiff);
		newB->shiftRightSize (resolutionDiff);
 		// Copy the old data to the low pass position
 		GO_SIGNAL3D_EACHELEMENT_2((*__ptr_target = *__ptr), (*b), (*newB), T, T);		
		newB->shiftLeftSize (resolutionDiff);
  		newB->shiftRightDiff (resolutionDiff);
		mem_mutex.lock();
		total_memory_usage += newB->memoryUsage();
		mem_mutex.unlock();
		// b->destroy();
    	// delete b;
		removeBlock (blockNumber);
		// everywhere the same -- wrong, FIXME --> FIXED?
#ifdef BLOCKSTORE_USE_ARRAY		
   		blockAddr[blockNumber] = (void*)newB;
#else
#ifdef BLOCKSTORE_USE_HASHTABLE
		blockAddr.add(blockNumber,(void*)newB);
#else
#ifdef BLOCKSTORE_USE_HASHCACHE
		blockAddr.add(blockNumber,(void*)newB);
#endif
#endif
#endif
			
	} else {
		goError::note ("goBlockStore::prepareBlockResolutionRaise()","refCount for block should be zero, but isn't! Segfault is immanent ...");
	}
	
	pm->refCountRelease();	
	// blockMutex.unlock();
    return newB;
}

/****** DOES NOT PERFORM SAFETY CHECKS! MAKE SURE YOUR SEATBELT IS FASTENED! ******/
template <class T>
void
goBlockStore<T>::removeBlock (goSize_t blockNumber)
{
#ifdef BLOCKSTORE_USE_HASHTABLE
  goSignal3D<T> *b;

  // pm->unSet (blockNumber);
  blockMutex.lock();		
  b = (goSignal3D<T>*)blockAddr.remove (blockNumber);
  blockMutex.unlock();
  if (blockAddr.fail())
    {
      char number[256];
      sprintf(&number[0], "%d", blockNumber);
      goString s;
      s = "Could not remove block number ";
      s += &number[0];
      goError::print("goBlockStore::removeBlock()",s);
    }
  else {
		mem_mutex.lock();
		total_memory_usage -= b->memoryUsage();
		mem_mutex.unlock();
		b->destroy();
    	delete b;
  }
#endif
  
#ifdef BLOCKSTORE_USE_HASHCACHE
  goSignal3D<T> *b;
  // pm->unSet (blockNumber);
  blockMutex.lock();
  b = (goSignal3D<T>*)blockAddr.remove (blockNumber);
  if (blockAddr.fail())
    {
      char number[256];
      sprintf(&number[0], "%d", blockNumber);
      goString s;
      s = "Could not remove block number ";
      s += &number[0];
      goError::print("goBlockStore::removeBlock()",s);
      blockMutex.unlock();
    }
  else 
  {
	blockMutex.unlock();
	mem_mutex.lock();
	total_memory_usage -= b->memoryUsage();
	mem_mutex.unlock();
	b->destroy();
    	delete b;
  }
#endif

#ifdef BLOCKSTORE_USE_ARRAY
	goSignal3D<T>* b;
	b = (goSignal3D<T>*)blockAddr[blockNumber];
	if (b)
	{
		// pm->unSet (blockNumber);
		mem_mutex.lock();
		total_memory_usage -= b->memoryUsage();
		mem_mutex.unlock();
		blockMutex.lock();
		blockAddr[blockNumber] = 0;
#if _GODEBUG >= 4		
		cout << getClassName() << ": calling delete operator on block " << blockNumber << endl;
#endif		
		b->destroy();
		delete b;
		blockMutex.unlock();
	}
#endif
}

template <class T>
bool
goBlockStore<T>::removeSomeBlocks ()
{
	goSize_t newMem;					// memory we aim at 
	newMem = max_memory_usage - max_memory_usage >> 2;	
										// leave 25% of max	memory usage free to avoid thrashing
										// back into this method
#ifdef BLOCKSTORE_USE_HASHCACHE		
    goArray<goSize_t> keys;
    goArray<void*>    values;
	goUInt8 removalFlag;
	removalFlag = Vol::PM_NO_FLAGS;
	bool leaveLoop = false;
	do {
 	   	keys.resize(0);
   	 	values.resize(0);
#if _GODEBUG >= 5
		cout << "blockStore: calling blockAddr.removeSome()" << endl;
		cout << "\tremovalFlag = " << (int)removalFlag << "\n";
#endif
		blockMutex.lock();
   	 	blockAddr.removeSome (keys, values, 
							  removalFlag, *pm);
		blockMutex.unlock();
#if _GODEBUG >= 5
		cout << "blockStore: back from blockAddr.removeSome()" << endl;
#endif
	  	goIndex_t i;
    	// Delete the blocks 
    	goSignal3D<T>* block;
    	for (i = 0; i < keys.getSize(); i++)
    	{
#if _GODEBUG >= 5
			cout << "blockStore: removing block " << keys[i] << " pointer " << values[i] << endl;
#endif
			// pm->unSet (keys[i]);
			block = (goSignal3D<T>*)values[i];
			mem_mutex.lock();
			total_memory_usage -= block->memoryUsage();
			mem_mutex.unlock();
#if _GODEBUG >= 5
			cout << "\tblockStore: pm->unsetting the block and deleting the pointer" << endl;
#endif
			block->destroy();
			delete block;
    	}
		if (!pm->testProtection(removalFlag,Vol::PM_LESS_PROTECTED))
			removalFlag++;
		else {
			if ( (removalFlag & Vol::PM_PREDICTED) != 0 )
				leaveLoop = true;  // no more to delete
			else
				{
					removalFlag = Vol::PM_NO_FLAGS | Vol::PM_PREDICTED;
				}
		}
#if _GODEBUG >= 5
		cout << "total memory size = " << total_memory_usage << ", newMem = " << newMem << "\n";
#endif
	} while ( (total_memory_usage > newMem) && (!leaveLoop) );
	 
#if _GODEBUG >= 1
    cout << getClassName() << " removed " << keys.getSize() << " Blocks from memory." << endl;
#endif
    return (keys.getSize() > 0);
#endif

#ifdef BLOCKSTORE_USE_HASHTABLE
	return false;
#endif

#ifdef BLOCKSTORE_USE_ARRAY
	// remove blocks that are present and have no flags set
	// remove blocks that are present and are LESSER_PROTECTED
	// remove blocks that are present and are LESS_PROTECTED
	// remove blocks that are present, PREDICTED and LESSER_PROTECTED
	// remove blocks that are present, PREDICTED and LESS_PROTECTED
												
	goSize_t i;
	goUInt8 flag;
	flag = (goUInt8)(Vol::PM_NO_FLAGS); 
	// Check through all blocks, until enough memory is freed or there are no more blocks we can
	// possibly free
	// blockMutex.lock();  // is called from removeBlock() !
	while ( !(pm->testProtection(flag,Vol::PM_PROTECTED)) && (total_memory_usage > newMem) )
	{
// #if _GODEBUG >= 2
		cout << getClassName() << "::removeSomeBlocks(): removing " << (int)flag << "-tagged blocks" << endl;
// #endif
		for (i = 0; (i < maxBlocks) && (total_memory_usage > newMem); i++)
		{
			// refcount
			bool R = pm->removalCheck(i);	// locks pm
			// Is block present and not referenced and not scheduled for loading?
			if ( R && 
				 (!pm->isScheduled (i)) )
			{
				// Block is tagged with the current flag? => delete
				if ( pm->getFlag(i) == flag ) 
				{
					pm->removalRemoved(i);
					this->removeBlock(i);				// remove block
					pm->removalRelease();				// unlocks pm
#if _GODEBUG >= 4					
					cout << getClassName() << " removed one block\n";
					cout << "\tBlock " << i << " flag " << (int)flag << "\n";
					cout << "\tCheck: " << (int)pm->getFlag(i) << endl;
#endif
					pm->setProtection (Vol::PM_NO_FLAGS, i);
					// pm->setFlag (Vol::PM_NO_FLAGS, i);	// unset all flags
				}	
				else
					pm->removalRelease();    // unlocks pm
			}
			else
				pm->removalRelease();		 // unlocks pm
		}
		if (flag == Vol::PM_LESS_PROTECTED)
		{
// #if _GODEBUG >= 3
			cout << getClassName() << "::removeSomeBlocks(): I'm in shit now... starting to remove PM_PREDICTED blocks..." << endl;
// #endif

			// Start again, just this time with the PREDICTED marked blocks
			flag = Vol::PM_NO_FLAGS | Vol::PM_PREDICTED;
		} else
		{
			flag++;
		}
	}
	// blockMutex.unlock();
//#if _GODEBUG >= 2
	cout << getClassName() << "::removeSomeBlocks() new memory usage: " << total_memory_usage << endl;
//#endif
	// return false always, since the array version of removeSomeBlocks() removes as many blocks
	// as possible
	return false; 
#endif
}

template <class T>
bool
goBlockStore<T>::removeSingleBlock (goSize_t blockNumber)
{
	bool retval;
	retval = pm->removalCheck(blockNumber);  // locks pm 
	if ( retval )
	{
#if _GODEBUG >= 4
		cout << getClassName() << ": removeSingleBlock " << blockNumber << ", flag " << flag << endl;
#endif		
		pm->removalRemoved(blockNumber);  // unsets the presence bit without locking the pm
		removeBlock (blockNumber);
	}
	pm->removalRelease();
	return retval;
}


template <class T>
goSignal3D<T>* 
goBlockStore<T>::getBlock (goSize_t blockNumber)
{
    goSignal3D<T>* s = 0;
	// The same for all 3 possible structures
	if (!pm->refCountInc(blockNumber))
	{
		goError::print("goBlockStore::getBlock()","************ REFCOUNT LIMIT REACHED *******"); 
		lastFailed = true;
		return 0;
	}
    s = (goSignal3D<T>*)blockAddr[blockNumber];
#ifdef BLOCKSTORE_USE_HASHCACHE	
    lastFailed = blockAddr.fail();
#else
	#ifdef BLOCKSTORE_USE_HASHTABLE
    lastFailed = blockAddr.fail();
	#else
		#ifdef BLOCKSTORE_USE_ARRAY
		lastFailed = (s == 0);
		#endif
	#endif	
#endif	
	return s;	

}


template <class T>
void
goBlockStore<T>::releaseBlock (goSize_t blockNumber)
{
	pm->refCountDec (blockNumber);
}

template<class T>
goSize3D& 
goBlockStore<T>::getBlockSize()
{
    return blockSize;
}

template<class T>
goSize_t
goBlockStore<T>::memoryUsage()
{
    return total_memory_usage;
}

}

template class Vol::goBlockStore <goInt8>;
template class Vol::goBlockStore <goUInt8>;
template class Vol::goBlockStore <goInt16>;
template class Vol::goBlockStore <goUInt16>;
template class Vol::goBlockStore <goInt32>;
template class Vol::goBlockStore <goUInt32>;
template class Vol::goBlockStore <goFloat>;
template class Vol::goBlockStore <goDouble>;

