#ifndef GO3DCOMPRESSED_H
#define GO3DCOMPRESSED_H

#include <go3dblock.h>
#include <go3dsubblock.h>
#include <go3dcoder.h>
#include <go3dmultistagest.h>
#include <gotypes.h>
#include <goarray.h>
#include <gostring.h>

#include <gocache.h>
#include <gocacheable.h>

#include <time.h>

#include <fstream.h>

/*!
 * Compressed representation of 3D data.
 * To compress an uncompressed volume, use <CODE>compress()</CODE>
 * or <CODE>readAndCompress()</CODE>.
 * <CODE>readAndCompress()</CODE> reads the data subblock-wise from
 * a file and compresses them. It does not read the whole data set at once.
 * You have to set a <CODE>go3DCoder</CODE> using <CODE>set3DCoder()</CODE>
 * before anything can be compressed.
 * Access to the blocks via <CODE>get3DBlock()</CODE> is cached with a 
 * <CODE>goCache</CODE> structure. Experiment with different cache sizes!
 * An optimal coder should be tried as well as a quantizer. Compare the
 * performance with/without quantization and optimal coding!
 * \note Use <CODE>go3DCoderSubBand</CODE> as coder. Other coders are
 * supported, but all tests were taken using the subband coder.
 * If others do not work, blame yourself or go and learn C++.
 * Do not use cache when this class is used with a shear warp renderer.
 * @author Christian Gosch
 */
template< class T,class CODE_T >
class
go3DCompressed {
 public:
  go3DCompressed ();
  virtual ~go3DCompressed ();

  void printTimes () 
    {
      cout << "Transform time: " << transformTime / (float)CLOCKS_PER_SEC << "s" << endl;
      cout << "Coder time: " << coderTime / (float)CLOCKS_PER_SEC << "s" << endl;
    }
  /*!
   * Split whole volume block into subblocks of size <CODE>blockSize</CODE>
   * and compress it.
   */
  void compress (go3DBlock<T> *volume);
  /*!
   * Write the compressed volume data into stream f, while writing needed index
   * data into streams indexFile (relative band indices) and 
   * blockIndexFile (length of each block's stream).
   * parameterFile is the file that is filled with the coder parameters during the
   * write process.
   */
  void write (ofstream &f, 
	      ofstream &indexFile, 
	      ofstream &blockIndexFile,
	      ofstream &parameterFile);

  /*!
   * Not quite that much under development anymore.
   * Add stage <CODE>stage</CODE> to the streams for blocks 
   * blockIndex1 to blockIndex2
   * from file stream f.
   * \note You <STRONG>have</STRONG> to call preloadIndices() before you can call
   * this method (of course you could try, but you will get exceptions; you do not
   * want that unless your name is Bill Gates).
   * \note It is assumed that the stage to be loaded was not yet loaded and
   * is the next higher band! So, for each block, stages can only be read in
   * incremental order, starting at 0.
   * \todo Write a method for reading up to a given stage from the whole 
   * file without having to read the whole file first (as it is now with read()).
   */
  void readBlockStage (ifstream &f, 
		       goSize_t blockIndex1, 
		       goSize_t blockIndex2,
		       goSize_t stage);
  /*!
   * Read stage <CODE>stage</CODE> from the given file names.
   * Read from block <CODE>blockIndex1</CODE> to block <CODE>blockIndex2</CODE>.
   * @see readBlockStage (ifstream, goSize_t, goSize_t, goSize_t)
   */
  void readBlockStage (goSize_t blockIndex1, 
		       goSize_t blockIndex2,
		       goSize_t stage);
  /*!
   * Set the file names:
   * \li \c volume:
   *   Name of the file containing the compressed volume data.
   * \li \c index:
   *   Name of the file containing the relative indices to each subband.
   * \li \c blockindex:
   *   Name of the file containing the block stream's lengths.
   * \li \c parameter:
   *   Name of the file containing the coder parameters for the Golomb coder
   *   that is used by the sub band coder.
   */
  void setFileNames (const char* volume, 
		     const char* index, 
		     const char* blockindex,
		     const char* parameter);
  void preloadIndices ();

  /*!
   * Read the compressed volume data from stream f, while reading needed index
   * data from streams indexFile (relative band indices) and 
   * blockIndexFile (length of each block's stream).
   */
  void read ();
  /*!
   * Read a volume from stream f and, while reading, compress it into this object.
   * This is really slow and needs to be optimised!
   * \todo This has a bug when volumes are not of dimensions which are
   * multiples of the block's dimensions. See source code of this
   * method (the memory allocation thing).
   * \todo See DANGER remark in source code
   */
  void readAndCompress (ifstream &f, goSize_t xs, goSize_t ys, goSize_t zs);
  /*!
   * Default: 16x16x16
   * MUST be a power of two!
   */ 
  void setBlockSize (go3DSize_t &s) 
    { 
      blockSize = s;
      int count = 0;
      while (s.x != 1) {
	s.x = s.x >> 1;
	count++;
      }
      blockSizeLog.x = count;
      count = 0;
      while (s.y != 1) {
	s.y = s.y >> 1;
	count++;
      }
      blockSizeLog.y = count;
      count = 0;
      while (s.z != 1) {
	s.z = s.z >> 1;
	count++;
      }
      blockSizeLog.z = count;
      
      trans.destroy();
      trans.make (blockSize.x, blockSize.y, blockSize.z);
      returnBlock.destroy();
      returnBlock.make (blockSize.x, blockSize.y, blockSize.z);
      returnBlockDiffX = returnBlock.getXDiff();
      returnBlockDiffY = returnBlock.getYDiff();
      returnBlockDiffZ = returnBlock.getZDiff();
    }

  /*!
   * Sets the size of the used block cache in number of blocks.
   * @see goCache
   */
  inline void setCacheSize (goSize_t s) { cache.setSize(s); }

  /*!
   * Sets the number of stages for the wavelet
   * transform in case a subband coder is used. 
   * Default: 1
   */
  void setStages (goSize_t s);
  /*!
   * @return Number of stages used to transform the data
   */
  inline goSize_t getStages () { return stages; }

  /*!
   * @return Size of the blocks (not the whole 3d block!)
   */
  inline const go3DSize_t& getBlockSize () { return blockSize; }
  /*!
   * @return \f$\log_{2}\f$ of the block size
   */
  inline const go3DSize_t& getBlockSizeLog() { return blockSizeLog; }

  void set3DCoder (go3DCoder<CODE_T> *c);

  void add3DBlock (go3DBlock<T>*);
  go3DBlock<T>* get3DBlock (goIndex_t blockIndex, goSize_t stage = 1);

  /*! 
   * x,y,z: coordinates of the BLOCK. DO NOT USE BOTH GET FUNCTIONS
   * ON ONE VOLUME (CACHE WORKS DIFFERENTLY).
   * EXPERIMENTAL. 
   */
  go3DBlock<T>* get3DBlock (goIndex_t blockIndex, goIndex_t x, goIndex_t y, goIndex_t z);

  /*!
   * @return Size of the coded streams in number of bytes.
   */
  goSize_t getSize ();

  /*!
   * @return Number of blocks in the compressed volume.
   */
  goSize_t getNumberOfBlocks () { return codedStreams.getSize() - 1; }

 protected:
  go3DMultiStageST<T,CODE_T>  	ST; 
  go3DCoder<CODE_T>	*coder;
  goArray<void*>	codedStreams;
  go3DSize_t		blockSize;
  go3DSize_t		blockSizeLog;
  
  
  void destroyCodedStreams ();

 private:
  void transform (go3DBlock<T>*);
  void encode ();
  void decode (goIndex_t blockIndex, go3DBlock<CODE_T> *b, goSize_t stage = 1);
  void reconstruct (go3DBlock<CODE_T> *b);

  go3DBlock<CODE_T>		trans;
  go3DBlock<T>			returnBlock;
  goPtrdiff_t			returnBlockDiffX;
  goPtrdiff_t			returnBlockDiffY;
  goPtrdiff_t			returnBlockDiffZ;
  goCache< go3DBlock<T> >	cache;

  goSize_t stages;

  clock_t transformTime, coderTime;

  /*! 
   * Indices of each detail block relative to each codedStream.
   * Set as <CODE>indexArray</CODE> for the subband coder. 
   * It contains offsets to each subband relative to each block's start.
   * The first entry for each stage is the offset
   * pointing to the start of the first detail subband.
   * The last entry for each stage is equal to the size of the whole block's stream
   * in CODE_T-type words.
   * For each block, there are stages * 8 entries.
   * See source code for 
   * <CODE>go3DCoderSubBand::encode()</CODE> and the accompanying macros
   * for exact details.
   * This array is filled while reading a volume in preloadIndices() or 
   * while encoding a volume
   * with a <CODE>go3DCoderSubBand</CODE>.
   */
  goArray<goSize_t>		indexArray;

  /*!
   * Contains the Golomb-Rice parameters for the Golomb coder used by the
   * sub band coder. It is filled by the sub band coder when encoding, and needed
   * when decoding.
   * @see <CODE>go3DCoderSubBand</CODE>
   */
  goArray<unsigned long>	coderParameter;
  /*!
   * Offset to each block's bitstream in the volume (including block 0!)
   * <STRONG>in number of words of type <CODE>CODE_T</CODE> (as usual), NOT IN BYTES!
   * </STRONG>.
   * This array is filled while reading a volume in preloadIndices().
   */ 
  goArray<goSize_t>		blockOffset;
  /*!
   * Length of each subband stream in each block. This array is filled while 
   * reading a volume.
   * The length of the 0th subband (low pass band) is <STRONG>included</STRONG>.
   * For each block, there are stages + 1 entries.
   * This array is filled while reading a volume in preloadIndices().
   */
  goArray<goSize_t>		stageLength;
  /*!
   * Relative offset to each <STRONG>stage</STRONG> in each block (including
   * the 0th stage, i.e. the lowpass band).
   * For each block, there are <CODE>stages</CODE> entries.
   */
  goArray<goSize_t>		stageIndex;
  
  /* File names */
  /*!
   * Name of the file containing the compressed volume.
   */
  goString			volumeFileName;
  /*!
   * Name of the file containing the coder parameters for the Golomb coder used by
   * the sub band coder.
   */
  goString			parameterFileName;
  /*!
   * Name of the file containing the index numbers needed by the subband coder.
   */
  goString			indexFileName;
  /*!
   * Name of the file containing the lengths of each block's bitstream in
   * number of words of type <CODE>CODE_T</CODE>.
   */
  goString			blockIndexFileName;
};

#endif





