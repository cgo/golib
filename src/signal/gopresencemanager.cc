#include <gopresencemanager.h>
#include <gotypes.h>
#include <goerror.h>
#include <goresolutionmanager.h>
#include <goexception.h>
#include <config.h>

namespace Vol {

template <class T>
goPresenceManager<T>::goPresenceManager()
    :  goTimerObject()
{
    setClassName ("goPresenceManager");
    presence = 0;
    rm = 0;
    file = 0;
    store = 0;
	rmFlags.resize(0);
}

template <class T>
goPresenceManager<T>::~goPresenceManager()
{
  if (presence)
    delete presence;
  if (file)
    file->closeTrans();
  signalPointers.resize(0);
  blockNumbers.resize(0);
  resolutions.resize(0);
  rmFlags.resize(0);
  refCount.resize(0);
}

template <class T>
bool
goPresenceManager<T>::init ()
{
    /*
     * Set filename and open the file
     */
    goString s;
    if (!file)
	{
	    s = "goPresenceManager::init() no file set";
	    throw goExceptionString(s);
	    // throw goExceptionString("test");
	    return false;
	}
    if (!rm)
	{
	    s = "goPresenceManager::init() no resolution manager set";
	    throw goExceptionString(s);
	    return false;
	}
    if (!store)
	{
	    s = "goPresenceManager::init() no block store set";
	    throw goExceptionString(s);
	    return false;
	}
    s = "Opening volume file";
    goError::note("goPresenceManager<T>::init()",s);
    // s = "Setting file type to bandwise";
    // goError::note("goPresenceManager<T>::init()",s);
    
    /*
     * Initialize secondary storage file
	 * Not needed anymore
     */ 
    // file->setTransFileType (goVolumeFile<T>::BANDWISE);		// assume blockwise file type
    file->setPresenceManager (this);
    if (!file->openTrans())
	{
	    s = "goPresenceManager<T>::init() can not open volume file";
	    throw goExceptionString(s);
	    return false;
	}
	file->setBlockStore (store);
	
    // Set this object as presence manager for the file. To enable multi threaded loading.
    // The file object sets the presence bits as soon as a block is loaded and available.
    
    /*
     * Make presence array according to volume size indicated by 
     * file.
     */
    goSize_t sz = file->getFileInfo().blocks.x *
	file->getFileInfo().blocks.y *
	file->getFileInfo().blocks.z;
    presence = new goBitArray(sz);
	presence->clear();
	
	/*
	 * Make Flags array of the same size (so we have (number of blocks) * 5 bits of storage for block information)
	 */
    // rmFlags = new goNibbleArray(sz);
	// rmFlags->fill (0);	
	rmFlags.resize (sz);
	rmFlags.byteFill (0);

	refCount.resize(sz);
	refCount.byteFill (0);

    /*
     * Initialize resolution manager.
     */
    rm->setNumberOfBlocks(sz);
    rm->setMinResolution (file->getFileInfo().stages);
    rm->init();

    /*
     * Initialize block store.
     */
    store->setBlockSize (file->getFileInfo().blockSize);
	store->setMaxBlocks (sz);
    store->init();

    /*
     * Clear presence bits for all blocks.
     */
    presence->clear();
    return true;
}

template <class T>
bool
goPresenceManager<T>::isPresent(goSize_t blockNumber)
{
	bool retval;
	// Sollte eigentlich gelockt sein, zum Schutz bei goBlockStore::removeSomeBlocks() 
	// gibt aber manchmal deadlock :-(((((
	// FIXME
	 lock();   
	retval = ((*presence)[blockNumber] == 1);
	 unlock();
  	return retval;
}

template <class T>
void
goPresenceManager<T>::makePresent (goSize_t blockNumber, goInt8 resolution)
{
    
    /* 
     * Get resolution information from the resolution manager, allocate block memory, and
     * load the needed levels from the file.
     */
    if (!isPresent(blockNumber)) 
	{
	    // int res = rm->getResolution (blockNumber);
	    rm->setResolution (blockNumber, resolution);
	    int maxRes = file->getFileInfo().stages;
	    goSignal3D<T>* block = store->createBlock (blockNumber, maxRes - resolution);
	    file->readTransBlock (blockNumber, block, resolution);
	    // presence->set(blockNumber);
	} 
}

template <class T>
void
goPresenceManager<T>::makePresent (goArray<goSize_t>& __blockNumbers, goArray<int>& __resolutions, bool waitUntilLoaded)
{

    /* 
     * Get resolution information from the resolution manager, allocate block memory, and
     * load the needed levels from the file.
     */
	arrayMutex.lock(); 
    signalPointers.resize(0);
    blockNumbers.resize	 (0);
    resolutions.resize	 (0);
    resolutionsMin.resize(0);
    goIndex_t i;
    int maxRes 			 = file->getFileInfo().stages;
    goSignal3D<T>* block = 0;
    int resolution 		 = 0;
    for (i = 0; i < __blockNumbers.getSize(); i++)
	{
	    // Muss Block geladen werden / ist richtige Aufloesung noch nicht da?
	    resolution = __resolutions[i];
		int oldRes  = rm->getResolution (__blockNumbers[i]);
		goSize_t blockNumber = __blockNumbers[i];

		// New block to be loaded
		if (!isPresent(blockNumber))
		{
			if (tryScheduleBlock(blockNumber))
			{
				// setScheduled(blockNumber);
		    	blockNumbers += blockNumber;
			    resolutions  += resolution;
			    resolutionsMin += 0; // to read the whole block up to __resolutions[i]
			    block = store->createBlock (blockNumber, maxRes - resolution);
		   	 	signalPointers += (void*)block;
		    	// Setze jetzige Aufloesung im resolution manager
		    	rm->setResolution (__blockNumbers[i], __resolutions[i]);
			}
		} 
		else
		// Block already existing, but in the wrong resolution and not referenced. 
		if (resolution != oldRes)	
		{
			// Reschedule only if the block is not marked as referenced
			// through the reference counter AND is not already scheduled
			if (tryScheduleBlock(blockNumber))
			// Block ist refcount==0, aber presence==1, kann also gelesen werden.
			// Ein Leser muß isScheduled() testen, wenn er sich auf Daten verlassen will.
			{
			    int resDiff = resolution - oldRes;
			    // Raise resolution
			    if (resDiff > 0)
				{
				    // Resize block memory and restructure old data

					
					// FIXME: prepareBlockResolution gibt nochn Fehler
					// Scheint zu gehen (25.11.2001)
					//while(!store->removeSingleBlock (__blockNumbers[i]))
					//{
					//	cout << getClassName() << ": removeSingleBlock returned false!!";
					//}
					// block = store->createBlock(__blockNumbers[i],maxRes - resolution);
				    block = store->prepareBlockResolutionRaise (__blockNumbers[i], resDiff);
					// In case something happens (it shouldn't), try again and 
					// print some information. This is just for being defensive.
					while (!block)
					{
						cout << getClassName() << ": trying to raise resolution again..." << endl;

						block = store->prepareBlockResolutionRaise (__blockNumbers[i], resDiff);
					}
				    // resolutionsMin[resolutionsMin.getSize() - 1] = oldRes + 1;
#if _GODEBUG >= 3
				    cout << "Scheduled block " << __blockNumbers[i] << " for adding " << resDiff << " stages." << endl;
#endif
					if (block)
					{
				    	signalPointers += (void*)block;
				   		blockNumbers += __blockNumbers[i];
				 	    resolutions  += __resolutions[i];
						// Set minimum stage to load to old resolution (that we already have)
						// to old resolution + 1. I.e., load from one stage above old data.
						// FIXME: Resolution raise funktioniert komischerweise noch nicht.
						// Scheint jetzt zu gehn (25.11.2001)
				    	resolutionsMin += oldRes + 1; 
						// resolutionsMin += 0;
		    			// Setze jetzige Aufloesung im resolution manager
					    rm->setResolution (__blockNumbers[i], __resolutions[i]);
					} else
					// This can not happen, but again, let's be paranoid.
					{
						goError::print(getClassName(),": Catastrophic error: block == 0");
						// presence->set (__blockNumbers[i]);
					}
				}
			    else 
				{
				    // Drop resolution
				    if (resDiff < 0)
					{
					    blockNumbers += __blockNumbers[i];
					    resolutions  += __resolutions[i];
					    resolutionsMin += 0; // to read the whole block up to __resolutions[i]
						// Make block temporarily unavailable
					    // presence->unSet (__blockNumbers[i]);
					    while(!store->removeSingleBlock (__blockNumbers[i]))
						{
							cout << getClassName() << ": removeSingleBlock returned false!!";
						}
					    block = store->createBlock (__blockNumbers[i], maxRes - __resolutions[i]);
					    signalPointers += (void*)block;
		    			// Setze jetzige Aufloesung im resolution manager
		    			rm->setResolution (__blockNumbers[i], __resolutions[i]);
					}
				    // Keep block / don't load or unset
				    else // resDiff == 0
					{
					    // do nothing
					}
				}
			}
		}
	}

	// createBlock muss auch refCount erhoehen, der lade thread muss es erniedrigen wenn er 
	// einen Block geladen hat. -- ah Bloedsinn isPresent wird ja erst gesetzt wenn der Block
	// geladen ist und er wird erst dann geloescht, wenn isPresent gesetzt ist :-)

	// Checks if blocks need to be deleted and deletes some if yes, based on the tags in rmFlags	
	// rmFlags is set by member functions of this class which get called by goViewManager
	startTimer();
	store->checkMemoryUsage();
	stopTimer();
#if _GODEBUG >= 1
	cout << getClassName() << ": Time to check memory/delete blocks: " << getTimerSeconds() << "s" << endl;
#endif
	// Always wake the loading thread when we are benchmarking:
#ifndef GO_BENCHMARK
    if (blockNumbers.getSize() > 0)
#endif
	{
	    // Returns immediately after taking the given blocks
	    file->readTransBlocks (blockNumbers, signalPointers, resolutions, resolutionsMin);
#if _GODEBUG >= 1
	    cout << getClassName() << ": blockstore size = " << store->memoryUsage() << endl;
#endif
	}
	arrayMutex.unlock();
}

template <class T>
void
goPresenceManager<T>::setResolutionManager (goResolutionManager *m)
{
    rm = m;
}

template <class T>
void
goPresenceManager<T>::setVolumeFile (goVolumeFile<T> *f)
{
    file = f;
}

template <class T>
void
goPresenceManager<T>::setBlockStore (goBlockStore<T> *s)
{
    store = s;
}

}

template class Vol::goPresenceManager <goInt8>;
template class Vol::goPresenceManager <goUInt8>;
template class Vol::goPresenceManager <goInt16>;
template class Vol::goPresenceManager <goUInt16>;
template class Vol::goPresenceManager <goInt32>;
template class Vol::goPresenceManager <goUInt32>;
template class Vol::goPresenceManager <goFloat>;
template class Vol::goPresenceManager <goDouble>;
