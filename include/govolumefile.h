#ifndef GOVOLUMEFILE_H
#define GOVOLUMEFILE_H

#include <gotypes.h>
#include <gostring.h>
#include <gosignal3d.h>
#include <gocluster3d.h>
#include <goarray.h>
#include <gostatusobject.h>
#include <gothread.h>
// #include <gosynchronized.h>
#include <goproducer.h>
#include <goblockstore.h>

#include <goqueue.h>

#include <fstream.h>


namespace Vol {

class readTransBlocks_arg
   {
     public:
      void*  volumeFile;
       goArray<goSize_t>*    blockNumbers; 
       goArray<void*>*	    signalPointers; 
       goArray<int>*	    stages; 
       goArray<int>*	    minStages; 
   };

// Calculates the correct size in bytes of a goVolumeFileInfo instance.
// Used for loading and writing transform volume files.
#define GO_VOLUMEFILEINFO_SIZE (sizeof(goSize_t) * 9 + sizeof(goInt32) * 4 + sizeof(goDouble) * 4)
/*!
 * Info structure for volume files (DWT transformed data, blocked)
 * @author Christian Gosch
 * @date 19.7.2001
 * @see goVolumeFile
 */
class
goVolumeFileInfo
{
 public:

  ///Version information
  goInt32	version;
  /// Size of the blocks
  goSize3D	blockSize;
  /// Actual size of the volume
  goSize3D	size;
  /// Number of blocks in x,y,z
  goSize3D	blocks;
  /// Number of stages used to transform the data
  goInt32	stages;
  /// 0: blockwise, 1: bandwise
  goInt32	fileType;

  /// Measure for the total energy contained in the signal
  goDouble	energy;
  /// Mean of the signal
  goDouble	mean;
  /// Measure for the minimum value of the signal
  goDouble	minimum;
  /// Measure for the maximum value of the signal
  goDouble  maximum;

  /// Data type. Saved for completeness.
  goInt32	dataType;
  
  /// NOT SAVED IN THE FILE Size of a block in bytes in the file stream
  streampos	blockLength;
  /// NOT SAVED IN THE FILE Size of this info in bytes in the file stream
  streampos	infoLength;
  /// NOT SAVED IN THE FILE Only has a meaning with blockwise saved files. Length of one block's stages in volume elements 
  /// NOT SAVED IN THE FILE (HP first, LP last)
  goArray<streamoff> stageLengths;
  /// NOT SAVED IN THE FILE - HP first, LP last -- only valid in bandwise files.
  //  Length of each band for all blocks.
  goArray<streamoff> totalBandLengths;

  void operator= (const goVolumeFileInfo& i);
};

ostream& operator<< (ostream& o, goVolumeFileInfo& info);

/*!
 * Reading and writing of volume files.
 * Linear (raw) files can be converted into transformed files. Transformed files are 
 * saved blockwise. The blocks are numbered linearly starting at block 0 at position (0,0,0).
 * The enumeration is in x-direction first, then y, then z.
 * The blocks are transformed using <code>goDWT</code> prior to writing.
 * The template parameter T is the scalar data type of the voxel data, being one of
 * - goInt8
 * - goUInt8
 * - goInt16
 * - goUInt16
 * - goInt32
 * - goUInt32
 * - goFloat
 * - goDouble
 *
 * Multimodal data is not yet handled.
 *  
 * @p The transformed files can have two different structures, which can be set with the setTransFileType() method.
 * Valid file types are 
 * - GO_VOLUMEFILE_BANDWISE
 * - GO_VOLUMEFILE_BLOCKWISE

 * @p BANDWISE files are structured like this:
 * The file structure is such that the header comes first, then the lowest (phi) resolution
 * of all blocks linearly, followed by the next higher detail of all blocks, then the next
 * ... until the last (highest) level of detail coefficients.

 * @p BLOCKWISE files are structured like this:
 * The header comes first, then the data is stored block-by-block:
 * First the highest detail coefficients, followed by all lower detail coefficients, last come the low pass coefficients.
 * See protected linear2Trans() methods for details.
 * @note If you just want to load a transformed volume file, it is sufficient to
 * simply open it with openTrans() after setting the file name with setTransFileName(). 
 * There is no need to set the volume size or anything since that is stored in the file.
 * You need to set the volume properties if you want to transform a file using 
 * linear2Trans().
 * @todo The conversion of very large data (say, 3000^3) still takes a lot
 * of memory. If you want to convert REALLY large data, you should 
 * slightly change the way in which the data is loaded. Currently,
 * N slices of the data are loaded (N being the edge length of a block)
 * into main memory and then transformed and written blockwise.
 * If N slices are very large, this could become a problem. Reading
 * The raw data blockwise takes up a lot of time, but if you have to do it
 * that way because of a lack of main memory, do it.<br>
 * 5.1.2002: Added the ability to convert raw files by lines of blocks.
 * This saves a lot of memory and should be sufficient unless a line by itself takes up the whole
 * main memory. If needed, add block-wise conversion. These methods are SLOWER than slab-wise conversion.
 * @author Christian Gosch
 * @date 17.7.2001
 */
template<class T>
class goVolumeFile : public goStatusObject, public goProducer, public goObjectBase
{
 public:
  goVolumeFile();
  virtual ~goVolumeFile();

  /*!
   * Set the name of the file that contains linear (raw) volume data.
   */
  void setLinearFileName (const char* filename);
  /*!
   * Set the name of the file that contains or will contain the transformed volume data.
   */
  void setTransFileName (const char* filename);

  /*!
   * Set file type of the transformed file (GO_VOLUMEFILE_[BANDWISE|BLOCKWISE]).
   * @param t File type
   */
  void setTransFileType (int t) { transFileType = t; }
  /*!
   * @return File type of the transformed file (GO_VOLUMEFILE_[BANDWISE|BLOCKWISE]).
   */
  int  getTransFileType () { return transFileType; }

  /*!
   * Blockstore is only used for read access. No changes are made to it.
   * @param b Pointer to the used block store.
   */
  void setBlockStore (goBlockStore<T> *b) { blockStore = b; }
  /*!
   * Set the size of the volume in voxels.
   */
  void setVolumeSize (goSize3D& sz);
  /*!
   * @see setVolumeSize(goSize3D&)
   */ 
  void setVolumeSize (goSize_t x, goSize_t y, goSize_t z);

  /*!
   * @see goVolumeFileInfo
   */
  const goVolumeFileInfo& getFileInfo () const { return info; }

  /*!
   * Converts the linear file into a transformed file 
   * using goDWT up to \em stage. The file names have to be set with setTransFileName() and setLinearFileName() prior
   * to calling this method.
   * @param blocksize Size of the blocks
   * @param stage Stage up to which to execute the transform
   * @param low_mem If true, the conversion is carried out block-line by block-line, instead of "slab-wise".
   *   		This saves main memory at the cost of conversion speed. 
   * @param slow_loading If true, no extra memory is used to speed up loading.
   *  This is useful if the blocks are very large.
   */
  bool linear2Trans(goSize3D& blockSize, int stage, bool low_mem=false, bool slow_loading=false);
  /*!
   * Initialize blockwise conversion.
   * @param blockSize Size of the blocks
   * @param stage Stage up to which to execute the transform
   * @param outfile Pointer to a ofstream object that represents an already open file
   */
  bool l2t_blockwise_init(goSize3D& blockSize, int stage, ofstream* outfile);
  /*!
   * Write one slab (a slice of blocks) to an open file stream
   * @param signal goSignal3D containing the slab
   * @param outfile Pointer to the open ofstream object
   * @param reset If true, just resets some internal variables and returns (do this before starting a new conversion)
   */
  bool l2t_blockwise_writeslab(goSignal3D<T>& signal, ofstream* outfile, bool reset=false);
  bool l2t_blockwise_writeblock(goSignal3D<T>& signal, ofstream* outfile, bool reset=false);
  /*!
   * Finish the current blockwise conversion.
   * The file is closed and extra information is written.
   * @param outfile The open ofstream object (will be closed after a call to this method).
   */
  bool l2t_blockwise_finish(ofstream* outfile);
  /*!
   * @see l2t_blockwise_init()
   */ 
  bool l2t_bandwise_init(goSize3D& blockSize, int stage);
  /*!
   * @see l2t_blockwise_writeslab()
   */
  bool l2t_bandwise_writeslab(goSignal3D<T>& signal, bool reset=false);
  /*!
   * @see l2t_blockwise_finish()
   */
  bool l2t_bandwise_finish();
  void writeBands(goSignal3D<T>& block, int stage, goArray<void*>& bandFiles);

  /*!
   * Overwrites the file info in the current (transformed) file with the given info.
   * The current file info object of this goVolumeFile is set to the new info.
   * The current file is always closed in the beginning of this method, so
   * you would have to reopen it with openTrans() after calling this method.
   * To overwrite a file info in a given file, set the filename with setTransFileName(), then 
   * call overwriteFileInfo() after creating and setting a file info object.
   * You can retrieve the file info from a file by first opening it with openTrans(), and then
   * getting the file info with getFileInfo().
   */
  void overwriteFileInfo(goVolumeFileInfo& i);
  /*!
   * Concatenates the temporary band files to the final BANDWISE file. Only used in
   * BANDWISE type files.
   */
  void concatBands(ofstream& outfile, goString *filenames, int nn);
  
  /*!
   * Read linear file from slice1 to slice2 into signal, which
   * must be of the correct dimensions.
   * The slices are read into slices 0 to (slice2 - slice1) in signal.
   * @note Not tested very much, but should work. Used by the 
   * linear2Trans method.
   */
  bool readLinear(goSignal3D<T>& signal, goSize_t slice1, 
		  goSize_t slice2, bool slow_loading=false);
  /*!
   * Read linear file area starting at x1,y1,z1 into signal.
   */
  bool readLinear(goSignal3D<T>& signal, goSize_t x1, goSize_t y1, goSize_t z1, bool slow_loading=false);
  /*!
   * Read whole linear file.
   * @note not implemented, maybe never will (not needed).
   */
  void readLinear();

  /*!
   * Read transform file from corner1 to corner2, read <strong>only</strong> <code>stage</code>.
   * @note not implemented, low priority
   */
  void readTrans(goSize3D& corner1, goSize3D& corner2, int stage);

  /*!
   * Read whole transform file.
   * @attention Deprecated, don't use
   * @note Reads the whole file and reconstructs all values to the maximum stage.
   * This is not what you normally want to do with large files.
   */
  bool readTrans(goCluster3D<T>& cluster);

  /*!
   * Opens the transformed volume file for reading.
   * After opening, the file header is read and stored, so the file info
   * can be retrieved using getFileInfo().
   * Call this before accessing blocks with readTransBlock().
   * @return True if the operation was successful, false otherwise.
   */
  bool openTrans();
  /*!
   * Closes the transformed volume file.
   * @return True if the operation was successful, false otherwise.
   */
  bool closeTrans();


  /*!
   * Reads the block with the linear number <code>blockNumber</code>
   * into <code>signal</code>, which must point to a goSignal3D which has the
   * <strong>correct dimensions</strong> for the reconstructed block of stage
   * <code>stage</code>. That means, since we use
   * a dyadic transform, the size of <code>signal</code> in one particular direction 
   * will usually have to be
   * [original blocksize] >> [(stages used in transform) - (stage)].
   * @param blockNumber Number of the block to be retrieved
   * @param signal Pointer to goSignal3D to take the data
   * @param stage Stage up to which the block should be reconstructed
   * @return Always true, no error checking is done.
   * @see goDWT
   * @note There are different implementations depending on the file type.
   */
  bool readTransBlock (goSize_t blockNumber, goSignal3D<T>* signal, int stage);

  /*!
   * Reads blocks which are ordered, the block numbers must be monotonically rising.
   * This method starts a new thread and returns immediately. 
   * The calling program can wait for the thread by calling the wait() method.
   * @param blockNumbers goArray containing the numbers of the blocks to be retrieved
   * @param signalPointers goArray containing the pointers to goSignal3D<T> to take the data
   * @param stages Stages up to which each block should be reconstructed
   * @param minStages Lowest stages to be read for each block (in case some stages have been loaded and
   * reconstructed before)
   * @note 27.11.2001 Bandwise and blockwise file types are implemented.
   */
  bool readTransBlocks (goArray<goSize_t>& blockNumbers, goArray<void*>& signalPointers, 
			goArray<int>& stages, goArray<int>& minStages);

 /*!
  * Waits for the thread started by readTransBlocks. Returns when the 
  * thread has finished. If no thread is running, returns immediately.
  */
  void wait ();

  /*! 
   * Used by the interpolation thread run simultaneously with the
   * load thread. Copies the borders of the loaded block to
   * neighbouring blocks.
   */
  // void interpolateBlocks (goArray<goSize_t>& blockNumbers);
  void interpolateBlocks ();

  /*!
   * Reads the high pass (HP), that is the detail coefficients.
   * @todo Not used, remove from source!
   * @note This member is deprecated and not used. It will disappear in 
   * future versions.
   */
  void readTransHP (goSignal3D<T> &signal, 
		    streampos bandLen_7, 
		    goSize_t blockNumber, 
		    int s, 
		    ifstream& transFile, 
		    streampos p, 
		    goSize_t blocks);

  /*!
   * For BANDWISE files
   * @param blockNumber Number of the block to be read
   * @param signal Pointer to the signal which will contain the block after the method returns
   * @param stage The stage up to which the block is to be loaded
   * @return True if successful
   */ 
  bool readTransBlock_bandWise (goSize_t blockNumber, goSignal3D<T>* signal, int stage);
  /*!
   * For BLOCKWISE files
   * @param blockNumber Number of the block to be read
   * @param signal Pointer to the signal which will contain the block after the method returns
   * @param stage The stage up to which the block is to be loaded
   * @return True if successful
   */ 
  bool readTransBlock_blockWise (goSize_t blockNumber, goSignal3D<T>* signal, int stage);
  /*!
   * For BANDWISE files. Recursive implementation for reading a block.
   * Used by readTransBlock_blockwise().
   * @param signal  goSignal3D which will contain the block after the method returns
   * @param f ifstream the method will read from. f has to point at the right position in the
   * stream prior to calling this method.
   * @param stage The stage up to which the block is to be loaded
   * @param stages Number of stages the file was originally transformed with.
   * @param readLast Indicates if the last band should be loaded (only for adding hight pass bands)
   */ 
  void blockwise_read (goSignal3D<T>& signal, ifstream& f, int stage, int stages, bool readLast);

  /*!
   * Reads a number of blocks listed in an internal queue from a BANDWISE type file.
   * @return True if successful
   */
  bool readTransBlocks_bandWise();
  /*!
   * Reads a number of blocks listed in an internal queue from a BLOCKWISE type file.
   * @return True if successful
   * @todo The code looks somewhat messy. There might be some opt. potential.
   */
  // bool readTransBlocks_blockWise (goArray<goSize_t>& blockNumbers, goArray<void*>& signalPointers, 
  //                       goArray<int>& stages, goArray<int>& minStages);
  bool readTransBlocks_blockWise (); 

  bool linear2Trans_bandWise (goSize3D& blockSize, int stage, bool block_by_block=false, bool slow_loading=false);
  bool linear2Trans_blockWise (goSize3D& blockSize, int stage, bool block_by_block=false, bool slow_loading=false);
  void l2t_blockwise_write (goSignal3D<T>& block, ofstream& f, int stage, int stages);

  /*!
   * Sets a pointer to a presence manager object which  will be used to set presence bits for loaded blocks.
   * This is added to enable a comfortable multi threaded architecture.
   * @param p Pointer to the goPresenceManager object to be used for flagging the presence of blocks.
   */
  void setPresenceManager (void* p) { this->pm = p; }

 protected:

  void setDataType();
  /*!
   * Positions transFile stream on the beginning of a given stage of a given block.
   * @param blockNumber Number of the block
   * @param stage Stage to position the stream pointer at.
   * @note For blockwise organization only.
   */
  void positionAt (goSize_t blockNumber, int stage);
  /*!
   * Version of move() for bandwise organized files. Moves stream pointer 
   * for blocksDiff number of blocks forward or backward. 
   * The band the stream pointer is positioned in currently has to be known.
   * The pointer can only be moved within the current band.
   */
  void move_bandWise (goIndex_t blocksDiff, int currentStage);
  /*!
   * Moves the stream pointer a given number of blocks further on.
   * @param blocksdif Number of blocks to hop over.
   * @note For blockwise organization only.
   */
  void move (goIndex_t blocksDiff);
  /*!
   * Moves the stream pointer blocksDiff blocks forward, then skips skipStages stages.
   * @param blocksDiff Number of blocks to move the stream pointer forward
   * @param skipStages Number of stages to be skipped within the block.
   * @note Requires the stream pointer to point at the beginning of a block (given if no single 
   * stage read has been issued before). If this is not given, use positionAt() before moving further on to 
   * bring the pointer to a block start point.
   */
  void move (goIndex_t blocksDiff, int skipStages);

  /// File name of the file containing the raw data.
  goString linearFileName;
  /// File name of the file containing the transformed data.
  goString transFileName;
  int transFileType;

  
  /// Stream handle to read transformed data.
  ifstream  transFile;

  /// File header of the transformed volume file.
  goVolumeFileInfo info;

  void wakeLoadThread();
 private:
  // Thread doing the loading from a file.
  goThread 		loadThread;
  goSemaphore		loadSema;
  goMutex		QMutex;
  goQueue<goSize_t>	blockQ;
  goQueue<void*>	blockPtrQ;
  goQueue<int>		stagesQ;
  goQueue<int>		minStagesQ;

  goQueue<goSize_t>	interpolationQ;
  goMutex		interpolationMutex;		
  goSemaphore		interpolationSema;

  readTransBlocks_arg loadThreadArg;
  goThread 		interpolationThread;
  goSemaphore 		blocksLoaded;
  goBlockStore<T>* 	blockStore;
  // goPresenceManager<T>*
  void* pm;

  /*
   * These are for bandwise transformation of a file:
   */
   goArray<void*> bandwise_bandFiles;  // ofstream*
   goString *bandwise_bandFileNames;
   
};

};

/*!
 * \example volumefile.cc
 * 	Example program to use some of the features of the goVolumeFile class.
 *  Enables a user to convert raw 3D data to a DWT transformed file format
 *  described in my Diplomarbeit. For details, refer to the source code.
 */

#endif

