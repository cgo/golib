#include <govolumefile.h>
#include <goerror.h>
#include <gocluster3d.h>
#include <gosubsignal3d.h>
#include <godwt.h>
#include <config.h>
#include <gosignalstat.h>
#include <godefs.h>

#include <gopresencemanager.h>

#include <stdio.h>   // tmpnam, remove function calls

namespace Vol {

// typedef off_t streamoff;   // workaround for g++ v3 not defining streamoff...

#define GO_VF_READTRANSBLOCKS_THREAD(__TYPE) { \
    /* goThread::setCancelType (0); */ \
    Vol::readTransBlocks_arg* arg = (Vol::readTransBlocks_arg*)p; \
    goVolumeFile<__TYPE>* file = (goVolumeFile<__TYPE>*)arg->volumeFile; \
    switch (file->getTransFileType()) \
	{ \
	case GO_VOLUMEFILE_BANDWISE: file->readTransBlocks_bandWise (); break; \
	case GO_VOLUMEFILE_BLOCKWISE: file->readTransBlocks_blockWise (); break; \
	default: goError::print("goVolumeFile::readTransBlocks()","There's an unknown volume file type. Are you sure you know what you are doing?");  \
	    return 0; \
	} \
    return 0; \
}



void*
readTransBlocks_thread_8 (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goInt8);
}
void*
readTransBlocks_thread_u8 (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goUInt8);
}

void*
readTransBlocks_thread_16 (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goInt16);
}
void*
readTransBlocks_thread_u16 (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goUInt16);
}

#if 0
void*
readTransBlocks_thread_32 (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goInt32);
}
void*
readTransBlocks_thread_u32 (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goUInt32);
}

void*
readTransBlocks_thread_f (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goFloat);
}
void*
readTransBlocks_thread_d (void* p)
{
	GO_VF_READTRANSBLOCKS_THREAD(goDouble);
}
#endif

#define GO_VF_INTERPOLATION_THREAD(__TYPE) { \
    /* goThread::setCancelType (0); */ \
    Vol::readTransBlocks_arg* arg = (Vol::readTransBlocks_arg*)p; \
    goVolumeFile<__TYPE>* file = (goVolumeFile<__TYPE>*)arg->volumeFile; \
    file->interpolateBlocks (); \
    return 0; \
}

void*
interpolation_thread_8 (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goInt8);
}
    
void*
interpolation_thread_u8 (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goUInt8);
}

void*
interpolation_thread_16 (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goInt16);
}

void*
interpolation_thread_u16 (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goUInt16);
}

#if 0
void*
interpolation_thread_32 (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goInt32);
}

void*
interpolation_thread_u32 (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goUInt32);
}
void*
interpolation_thread_f (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goFloat);
}

void*
interpolation_thread_d (void* p)
{
	GO_VF_INTERPOLATION_THREAD(goDouble);
}
#endif

/*
 * Dateikopf in info lesen
 */
static inline void readFileInfo (goVolumeFileInfo& info, ifstream& infile)
{
  streamoff off = GO_VOLUMEFILEINFO_SIZE;
  off *= -1;
  infile.seekg (off, ios::end);
  infile.read((char*)&info.version, sizeof(goInt32));
  infile.read((char*)&info.blockSize.x, sizeof(goSize_t));
  infile.read((char*)&info.blockSize.y, sizeof(goSize_t));
  infile.read((char*)&info.blockSize.z, sizeof(goSize_t));
  infile.read((char*)&info.blocks.x, sizeof(goSize_t));
  infile.read((char*)&info.blocks.y, sizeof(goSize_t));
  infile.read((char*)&info.blocks.z, sizeof(goSize_t));
  infile.read((char*)&info.size.x, sizeof(goSize_t));
  infile.read((char*)&info.size.y, sizeof(goSize_t));
  infile.read((char*)&info.size.z, sizeof(goSize_t));
  infile.read((char*)&info.stages, sizeof(goInt32));
  infile.read((char*)&info.fileType, sizeof(goInt32));
  infile.read((char*)&info.energy, sizeof(goDouble));
  infile.read((char*)&info.mean, sizeof(goDouble));
  infile.read((char*)&info.minimum, sizeof(goDouble));
  infile.read((char*)&info.maximum, sizeof(goDouble));
  infile.read((char*)&info.dataType, sizeof(goInt32));

  infile.seekg (0, ios::beg);
  /*
   * For blockwise files, calculate block's stage lengths in ELEMENTS:
   */ 
  info.stageLengths.resize (info.stages + 1);
  info.totalBandLengths.resize (info.stages + 1);
  goSize_t l = info.blockSize.x * info.blockSize.y * info.blockSize.z;
  // l >>= 3;	// l / 8
  // l *= 7;	// 7/8 von der Gesamtlänge
  info.stageLengths[info.stages] = (l >> 3) * 7;
  info.totalBandLengths[info.stages] = (l >> 3) * 7 * info.blocks.x * info.blocks.y * info.blocks.z;
  int i;
  for (i = info.stages - 1; i > 0; i--)
  {
	  l >>= 3;     // next stage 1/8 as large as previous
	  info.stageLengths[i]     = (l >> 3) * 7;
	  info.totalBandLengths[i] = (l >> 3) * 7 * info.blocks.x * info.blocks.y * info.blocks.z;
  }
  info.stageLengths[0] = l >> 3;
  info.totalBandLengths[0] = info.stageLengths[0] * info.blocks.x * info.blocks.y * info.blocks.z;

  cout << "Stage lengths: ";
  for (i = 0; i < info.stageLengths.getSize(); i++)
  {
	  cout << info.stageLengths[i] << ",";
  }
  cout << endl;
  cout << "Total band lengths: ";
  for (i = 0; i < info.totalBandLengths.getSize(); i++)
  {
	  cout << info.totalBandLengths[i] << ",";
  }
  cout << endl;

  cout << "readFileInfo results:" << endl;
  cout << info;
}

/*
 * Dateikopf schreiben, Daten stehen in info
 */
static inline void writeFileInfo (goVolumeFileInfo& info, ofstream& outfile)
{
  outfile.write((const char*)&info.version, sizeof(goInt32));
  outfile.write((const char*)&info.blockSize.x, sizeof(goSize_t));
  outfile.write((const char*)&info.blockSize.y, sizeof(goSize_t));
  outfile.write((const char*)&info.blockSize.z, sizeof(goSize_t));
  outfile.write((const char*)&info.blocks.x, sizeof(goSize_t));
  outfile.write((const char*)&info.blocks.y, sizeof(goSize_t));
  outfile.write((const char*)&info.blocks.z, sizeof(goSize_t));
  outfile.write((const char*)&info.size.x, sizeof(goSize_t));
  outfile.write((const char*)&info.size.y, sizeof(goSize_t));
  outfile.write((const char*)&info.size.z, sizeof(goSize_t));
  outfile.write((const char*)&info.stages, sizeof(goInt32));
  outfile.write((const char*)&info.fileType, sizeof(goInt32));
  outfile.write((const char*)&info.energy, sizeof(goDouble));
  outfile.write((const char*)&info.mean, sizeof(goDouble));
  outfile.write((const char*)&info.minimum, sizeof(goDouble));
  outfile.write((const char*)&info.maximum, sizeof(goDouble));
  outfile.write((const char*)&info.dataType, sizeof(goInt32));

  cout << "FileInfo written:" << endl;
  cout << info;
}


void
goVolumeFileInfo::operator=(const goVolumeFileInfo& i)
{
	version = i.version;
	blockSize = i.blockSize;
	size = i.size;
	blocks = i.blocks;
	stages = i.stages;
	fileType = i.fileType;
	energy = i.energy;
	mean = i.mean;
	minimum = i.minimum;
	maximum = i.maximum;
	blockLength = i.blockLength;
	infoLength = i.infoLength;
	stageLengths = i.stageLengths;
	totalBandLengths = i.totalBandLengths;
}


template<class T>
goVolumeFile<T>::goVolumeFile()
    : goStatusObject(), goProducer(), goObjectBase()
{
  setClassName ("goVolumeFile");
  info.size.x = 0;
  info.size.y = 0;
  info.size.z = 0;
  info.blockSize.x = 0;
  info.blockSize.y = 0;
  info.blockSize.z = 0;
  linearFileName.resize(0);
  transFileName.resize(0);
  setTransFileType (GO_VOLUMEFILE_UNKNOWN);
  pm = 0;
  bandwise_bandFiles.resize(0);
  bandwise_bandFileNames = 0;
  this->setDataType();
}

template<class T>
goVolumeFile<T>::~goVolumeFile()
{
  linearFileName.resize(0);
  transFileName.resize(0);
}

template<class T>
void
goVolumeFile<T>::setDataType()
{
	goError::print("goVolumeFile::setDataType()","No specialization for data type");
}

void
goVolumeFile<goInt8>::setDataType()
{
	info.dataType = 0;
}

void
goVolumeFile<goUInt8>::setDataType()
{
	info.dataType = 1;
}

void
goVolumeFile<goInt16>::setDataType()
{
	info.dataType = 2;
}

void
goVolumeFile<goUInt16>::setDataType()
{
	info.dataType = 3;
}

/*
void
goVolumeFile<goInt32>::setDataType()
{
	info.dataType = 4;
}

void
goVolumeFile<goUInt32>::setDataType()
{
	info.dataType = 5;
}

void
goVolumeFile<goFloat>::setDataType()
{
	info.dataType = 6;
}

void
goVolumeFile<goDouble>::setDataType()
{
	info.dataType = 7;
}
*/

template<class T>
void
goVolumeFile<T>::setLinearFileName(const char* filename)
{
  linearFileName = filename;
}

template<class T>
void
goVolumeFile<T>::setTransFileName(const char* filename)
{
    transFileName = filename;
    goString s;
    s = "transFileName set to ";
    s += filename;
    goError::note("goVolumeFile::setTransFileName()",s);
}

template<class T>
void
goVolumeFile<T>::setVolumeSize(goSize3D& sz)
{
  info.size = sz;
}

template<class T>
void
goVolumeFile<T>::setVolumeSize(goSize_t x, goSize_t y, goSize_t z)
{
  info.size.x = x;
  info.size.y = y;
  info.size.z = z;
}


template<class T>
void
goVolumeFile<T>::writeBands(goSignal3D<T>& block, int stage, goArray<void*>& bandFiles)
{
  /*
   * Fuer jedes Band ein file, Daten des Bandes dieses blocks
   * daran anhaengen.
   * concatBands haengt alle diese Dateien aneinander.
   */ 
  
  if (stage > 0)
  {
	  ofstream *bandFile;
	  bandFile = (ofstream*)bandFiles[stage];
      goSubSignal3D<T> sub;
      sub.setSize (block.getSizeX() >> 1, block.getSizeY() >> 1, block.getSizeZ() >> 1);
      sub.setDiff (block.getXDiff() << 1, block.getYDiff() << 1, block.getZDiff() << 1);
      sub.setParent (&block);
      sub.setPosition (1,0,0);
      sub.write (*bandFile);
      sub.setPosition (0,1,0);
      sub.write (*bandFile);
      sub.setPosition (1,1,0);
      sub.write (*bandFile);
      sub.setPosition (0,0,1);
      sub.write (*bandFile);
      sub.setPosition (1,0,1);
      sub.write (*bandFile);
      sub.setPosition (0,1,1);
      sub.write (*bandFile);
      sub.setPosition (1,1,1);
      sub.write (*bandFile);

      sub.setPosition (0,0,0);      
      writeBands (sub, stage - 1, bandFiles);
  } else {
      block.write (*((ofstream*)bandFiles[0]));
  }
}

template<class T>
void
goVolumeFile<T>::overwriteFileInfo(goVolumeFileInfo& i)
{
	transFile.close();
	ofstream outfile;
	outfile.open(transFileName.toCharPtr(), ios::bin|ios::out|ios::nocreate);
	if (outfile.fail())
	{
		goError::print("goVolumeFile::overwriteFileInfo()","Check file for existance!");
		return;
	}
	info = i;
  	streamoff off = GO_VOLUMEFILEINFO_SIZE;
 	off *= -1;
 	outfile.seekp(off, ios::end);
	writeFileInfo(info, outfile);
	outfile.close();
}

template<class T>
void
goVolumeFile<T>::concatBands (ofstream& outfile, goString *names, int nn)
{
  	int i;
	ifstream infile;
	goSize_t buffSize = 2 * 1024 * 1024;
 	char buffer[buffSize];
 	for (i = 0; i <= nn; i++)
 	{
 	    infile.open (names[i].toCharPtr(), ios::in|ios::binary);
		infile.seekg(0,ios::end);
		streampos L = infile.tellg();
		streampos rest = L;
		infile.seekg(0,ios::beg);
   	   	while(!infile.eof())
		{
			if (rest < buffSize)
			{
				infile.read((char*)&buffer[0], rest);
				outfile.write((char*)&buffer[0], rest);
				break;
			} else
			{
	  			infile.read((char*)&buffer[0], buffSize);
	    		outfile.write((const char*)&buffer[0], buffSize);
				rest -= buffSize;
			}
		}
      	infile.close();
    }
}

template<class T>
bool
goVolumeFile<T>::linear2Trans(goSize3D& blockSize, int stage, bool block_by_block, bool slow_loading)
{
    this->setBusy(true);
    switch (transFileType)
	{
	case GO_VOLUMEFILE_BANDWISE: return linear2Trans_bandWise (blockSize, stage, block_by_block, slow_loading); 
	case GO_VOLUMEFILE_BLOCKWISE: return linear2Trans_blockWise (blockSize, stage, block_by_block, slow_loading); 
	default: goError::print("goVolumeFile::linear2Trans()","Unknown file type"); 
	    this->setBusy (false);
	    return false;
	}
    this->setBusy(false);
}

template<class T>
bool
goVolumeFile<T>::readLinear(goSignal3D<T>& signal, goSize_t slice1, goSize_t slice2, bool slow_loading)
{
    this->setBusy (true);
  ifstream infile;
  infile.open(linearFileName.toCharPtr(), ios::in|ios::binary);
  if (infile.fail())
    {
      goString s;
      s = "Could not open file ";
      s += linearFileName;
      goError::print("goVolumeFile::readLinear()",s);
      this->setBusy(false);
      return false;
    }
  goSize_t sliceSize = info.size.y * info.size.x;
  // infile.seekoff(slice1 * sliceSize * sizeof(T), ios::beg);
  infile.seekg((streampos)(slice1 * sliceSize * sizeof(T)));
  goSize_t i, i2;
  i2 = slice2 - slice1;
  for (i = 0; i < i2; i++)
    {
      signal.readSlice(infile, i, slow_loading);
    }
  infile.close();
  this->setBusy(false);
  return true;
}

template<class T>
bool
goVolumeFile<T>::readLinear(goSignal3D<T>& signal, goSize_t x1, goSize_t y1, goSize_t z1, bool slow_loading) 
{
    this->setBusy (true);
  ifstream infile;
  infile.open(linearFileName.toCharPtr(), ios::in|ios::binary);
  if (infile.fail())
    {
      goString s;
      s = "Could not open file ";
      s += linearFileName;
      goError::print("goVolumeFile::readLinear()",s);
      this->setBusy(false);
      return false;
    }
  goSize_t sliceSize = info.size.y * info.size.x;
  goSize_t lineSize  = info.size.x;
  // infile.seekoff(slice1 * sliceSize * sizeof(T), ios::beg);
  // Auf Anfang
  // infile.seekg((streampos)((z1 * sliceSize + y1 * lineSize + x1) * sizeof(T)));
  goSize_t i, i2;
  goSubSignal3D<T> line;
  line.setParent(&signal);
  line.setSize(signal.getSizeX(), 1, 1);
  line.setDiff(signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
  for (i = 0; i < signal.getSizeZ(); i++)
  {
	// Auf Anfang der Schicht  
  	infile.seekg((streampos)(((z1 + i) * sliceSize + y1 * lineSize + x1) * sizeof(T)));
  	line.setPosition(0,0,i);
	  for (i2 = 0; i2 < signal.getSizeY(); i2++)
	  {
		  line.readSlice(infile, 0, slow_loading);
		  line.move(GO_DIRECTION_Y);
		  infile.seekg((streamoff)((info.size.x - line.getSizeX()) * sizeof(T)),ios::cur);
	  }
  }
  infile.close();
  this->setBusy(false);
  return true;
}

/*
 * Funktioniert noch nicht. Dateistruktur ist jetzt anders. Vergiss es.
 * goCluster3D gilt als ausgesondert. Wird nur noch zum Bestimmen der Blockgroessen verwendet.
 */
#if 0
template<class T>
bool
goVolumeFile<T>::readTrans (goCluster3D<T>& cluster)
{
  goSize_t x,y,z;
  ifstream infile;
  infile.open(transFileName.toCharPtr(), ios::in|ios::binary);
  readFileInfo (info, infile);
  
  cluster.setSize (info.size.x, info.size.y, info.size.z);
  cluster.setBlockSize (info.blockSize.x, info.blockSize.y, info.blockSize.z);
  cluster.update();
  
  goError::note("goVolumeFile::readTrans()","");
  cout << "\tBlocksize: (" << info.blockSize.x << "," << info.blockSize.y << "," << info.blockSize.z << ")\n";
  cout << "\tBlocks total: (" << info.blocks.x << "," << info.blocks.y << "," << info.blocks.z << ")\n";
  cout << "\tDWT stages: " << info.stages << endl;
  
  /*
   * Read data (all of it) into cluster
   */
  goSignal3D<T> *block;
  goDWT<T>	dwt;
  for (z = 0; z < info.blocks.z; z++)
    {
      for (y = 0; y < info.blocks.y; y++)
		{
	 	 for (x = 0; x < info.blocks.x; x++)
	 	   {
	  	    block = new goSignal3D<T>;
	   	   	block->make (info.blockSize.x, info.blockSize.y, info.blockSize.z);
	   	  	 /* Block lesen */
	   	  	 block->read (infile);
	   	  	 cluster.addBlockPtr (block);
	   	   	dwt.unHaar (*block, info.stages);
	       }
		}
    }
  infile.close();
  return true;
}
#endif

#ifdef MULTIPLE_THREADS 
#define GO_VF_OPENTRANS(__TYPE,readTransBlocks_thread_SUFFIX,interpolation_thread_SUFFIX) { \
  this->setBusy (true); \
  if (!pm) \
      { \
	  goString s; \
	  s = "No presence manager set!"; \
	  goError::print("goVolumeFile::openTrans()",s); \
	  this->setBusy (false); \
	  return false; \
      } \
  transFile.open(transFileName.toCharPtr(), ios::in|ios::binary); \
  if (transFile.fail()) \
    { \
      goString s; \
      s = "Error opening file ";\
      s += transFileName.toCharPtr();\
      goError::print("goVolumeFile::openTrans()",s);\
      this->setBusy (false);\
      return false;\
    }\
  readFileInfo (info, transFile);\
  if (info.fileType == 0) \
	  setTransFileType(GO_VOLUMEFILE_BLOCKWISE);\
  else setTransFileType(GO_VOLUMEFILE_BANDWISE);\
  info.blockLength = info.blockSize.x * info.blockSize.y * info.blockSize.z * sizeof(__TYPE);\
  info.infoLength  = GO_VOLUMEFILEINFO_SIZE;\
\
  cout << getClassName() << ": starting load and interpolation threads" << endl;\
  loadThreadArg.volumeFile     = (void*)this;\
  loadThread.create (readTransBlocks_thread_SUFFIX, (void*)&loadThreadArg, 1);\
  interpolationThread.create (interpolation_thread_SUFFIX, (void*)&loadThreadArg, 1); \
  this->setBusy (false); \
\
  return (!transFile.fail());\
}
#else
#define GO_VF_OPENTRANS(__TYPE,__DUMMY1,__DUMMY2) { \
  this->setBusy (true); \
  if (!pm) \
      { \
	  goString s; \
	  s = "No presence manager set!"; \
	  goError::print("goVolumeFile::openTrans()",s); \
	  this->setBusy (false); \
	  return false; \
      } \
  transFile.open(transFileName.toCharPtr(), ios::in|ios::binary); \
  if (transFile.fail()) \
    { \
      goString s; \
      s = "Error opening file ";\
      s += transFileName.toCharPtr();\
      goError::print("goVolumeFile::openTrans()",s);\
      this->setBusy (false);\
      return false;\
    }\
  readFileInfo (info, transFile);\
  if (info.fileType == 0)\
	  setTransFileType(GO_VOLUMEFILE_BLOCKWISE);\
  else setTransFileType(GO_VOLUMEFILE_BANDWISE);\
  info.blockLength = info.blockSize.x * info.blockSize.y * info.blockSize.z * sizeof(__TYPE);\
  info.infoLength  = GO_VOLUMEFILEINFO_SIZE;\
\
  cout << getClassName() << ": single threaded, not starting load and interpolation thread" << endl;\
  this->setBusy (false); \
\
  return (!transFile.fail());\
}
#endif 

template<class T>
bool
goVolumeFile<T>::openTrans ()
{
	goError::print("goVolumeFile::openTrans()","No specialization for this type!");
	return false;
}
bool
goVolumeFile<goInt8>::openTrans ()
{
	GO_VF_OPENTRANS(goInt8,readTransBlocks_thread_8,interpolation_thread_8);
}
bool
goVolumeFile<goUInt8>::openTrans ()
{
	GO_VF_OPENTRANS(goUInt8,readTransBlocks_thread_u8,interpolation_thread_u8);
}
bool
goVolumeFile<goInt16>::openTrans ()
{
	GO_VF_OPENTRANS(goInt16,readTransBlocks_thread_16,interpolation_thread_16);
}
bool
goVolumeFile<goUInt16>::openTrans ()
{
	GO_VF_OPENTRANS(goUInt16,readTransBlocks_thread_u16,interpolation_thread_u16);
}

template<class T>
bool
goVolumeFile<T>::closeTrans ()
{
  transFile.close();
  return (!transFile.fail());
}


template<class T>
void 
goVolumeFile<T>::readTransHP (goSignal3D<T> &signal, 
						      streampos bandLen_7, 
						      goSize_t blockNumber, 
			    			  int s, 
						      ifstream& transFile, 
						      streampos p, 
						      goSize_t blocks)
{
  if (s > 0)
    {
      goSubSignal3D<T> subSignal;
      subSignal.setParent (&signal);
      subSignal.setSize (signal.getSizeX(), signal.getSizeY(), signal.getSizeZ());
      subSignal.setDiff (signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
      transFile.seekg(p + (streampos)(bandLen_7 * blockNumber));
      // p += bandLen_7 * blocks;
      // bandLen <<= 3;
      subSignal.setPosition (1,0,0);
      subSignal.read (transFile);
      subSignal.setPosition (0,1,0);
      subSignal.read (transFile);
      subSignal.setPosition (1,1,0);
      subSignal.read (transFile);
      subSignal.setPosition (0,0,1);
      subSignal.read (transFile);
      subSignal.setPosition (1,0,1);
      subSignal.read (transFile);
      subSignal.setPosition (0,1,1);
      subSignal.read (transFile);
      subSignal.setPosition (1,1,1);
      subSignal.read (transFile);
      
      subSignal.shiftLeftSize (1);
      subSignal.shiftRightDiff (1);
      readTransHP (subSignal, 
		   (streampos)(bandLen_7 << 3), 
		   (goSize_t)blockNumber, (int)(s - 1), 
		   transFile, (streampos)(p + (streampos)(bandLen_7 * blocks)), (goSize_t)blocks);
    }
}


template<class T>
bool
goVolumeFile<T>::readTransBlock (goSize_t blockNumber, goSignal3D<T>* signal, int stage)
{
    this->setBusy (true);
    switch (transFileType)
	{
	case GO_VOLUMEFILE_BANDWISE: return readTransBlock_bandWise (blockNumber, signal, stage); break;
	case GO_VOLUMEFILE_BLOCKWISE: return readTransBlock_blockWise (blockNumber, signal, stage); break;
	default: goError::print("goVolumeFile::readTransBlock()","Seriously man, there's an unknown volume file type!!!"); 
	    this->setBusy (false);
	    return false;
	}
    this->setBusy (false);
}




template<class T>
bool
goVolumeFile<T>::readTransBlocks (goArray<goSize_t>& blockNumbers, goArray<void*>& signalPointers, 
				  goArray<int>& stages, goArray<int>& minStages)
{
    this->setBusy (true);

    int i;
    QMutex.lock();
    for (i = 0; i < blockNumbers.getSize(); i++)
	{
	    blockQ.add (blockNumbers[i]);
	    blockPtrQ.add (signalPointers[i]);
	    stagesQ.add (stages[i]);
	    minStagesQ.add (minStages[i]);
	}
    QMutex.unlock();
    wakeLoadThread();

    return true;
}

template<class T>
void
goVolumeFile<T>::wakeLoadThread()
{
#ifdef MULTIPLE_THREADS
    loadSema.inc();   // increment load-semaphore
#else
	switch(transFileType)
	{
		case GO_VOLUMEFILE_BLOCKWISE: readTransBlocks_blockWise(); break;
		case GO_VOLUMEFILE_BANDWISE: readTransBlocks_bandWise(); break;
		default: break;
	}
#endif
}

template<class T>
void
goVolumeFile<T>::wait()
{
    cout << getClassName() << ": wait() is not possible anymore!" << endl;
    return;
    //loadThread.join();
    //interpolationThread.join();
}

template<class T>
void
goVolumeFile<T>::interpolateBlocks ()
{
    //goIndex_t i;
    goInt32 xBlocks      = getFileInfo().blocks.x;
    goInt32 yBlocks      = getFileInfo().blocks.y;
    goInt32 zBlocks      = getFileInfo().blocks.z;
    goInt32 x,y,z;
    goSignal3D<T> *block;
    goSignal3D<T> *block2;
    goSize_t blockIndex;	// Index of this block
	goPresenceManager<T>* _pm;
	_pm = (goPresenceManager<T>*)pm;
#ifdef MULTIPLE_THREADS
    while (true)
#endif
	{
#ifdef MULTIPLE_THREADS
	    interpolationSema.dec();
#endif
	    while (!interpolationQ.isEmpty())
		{
		    interpolationMutex.lock();
		    blockIndex = interpolationQ.getHead();
		    interpolationQ.remove();
		    interpolationMutex.unlock();
		    // 		cout << "size: " << blockNumbers.getSize() << endl;
		    // 		cout << "wait block " << i << endl;

		    goIndex_t n;			// Block index of neighbour
			// Calculate x,y,z coordinates of the block in the grid of blocks
		    z = (goInt32)(blockIndex / (float)(xBlocks * yBlocks));
		    y = (goInt32)( (blockIndex - z * (xBlocks * yBlocks)) / (float)xBlocks);
		    x = blockIndex - (y * xBlocks + z * (xBlocks * yBlocks));
		    // cout << "interpolating block " << x << "," << y << "," << z << endl;
		    block  = (goSignal3D<T>*)blockStore->getBlock (blockIndex);
		    // block->interpolateBorders();
	    
		    if (x > 0)		
			{
			    n      = (goIndex_t)blockIndex - 1;					// left neighbour
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::RIGHT);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::LEFT);
				}
				blockStore->releaseBlock(n);
			}
		
		    if (x < (xBlocks - 1))
			{
			    n      = blockIndex + 1;							// right neighbour
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::LEFT);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::RIGHT);
				}
				blockStore->releaseBlock(n);
			}
		
		    if (y > 0)
			{
			    n	   = blockIndex - xBlocks;  					// top neighbour
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::BOTTOM);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::TOP);
				}
				blockStore->releaseBlock(n);
			}

		    if (y < (yBlocks - 1))
			{
			    n      = blockIndex + xBlocks;						// bottom neighbour
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::TOP);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::BOTTOM);
				}
				blockStore->releaseBlock(n);
			}
		
		    if (z > 0)
			{
			    n      = blockIndex - (xBlocks * yBlocks);			// front neighbour
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::BACK);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::FRONT);
				}
				blockStore->releaseBlock(n);
			}

		    if (z < (zBlocks - 1))
			{
			    n      = blockIndex + (xBlocks * yBlocks);			// back neighbour
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::FRONT);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::BACK);
				}
				blockStore->releaseBlock(n);
			}
		
		
		    if ( (z > 0) && (y > 0) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) - xBlocks;			// edge7
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE7);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE1);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (z > 0) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) + xBlocks;			// edge3
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE3);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE5);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (x < (xBlocks - 1)) && (y > 0) )
			{
			    n	= blockIndex + 1 - xBlocks;								// edge8
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE8);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE2);
				}
				blockStore->releaseBlock(n);
			}	
		
		    if ( (x < (xBlocks - 1)) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex + 1 + xBlocks;								// edge4
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE4);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE6);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (z < (zBlocks - 1)) && (y > 0) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) - xBlocks;			// edge5
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE5);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE3);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (z < (zBlocks - 1)) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) + xBlocks;			// edge1
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE1);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE7);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (x > 0) && (y > 0)  )
			{
			    n	= blockIndex - 1 - xBlocks;								// edge6
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE6);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE4);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (x > 0) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex - 1 + xBlocks;								// edge2
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE2);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE8);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (z > 0) && (x < (xBlocks - 1)) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) + 1;					// edge11
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE11);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE9);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (z < (zBlocks - 1)) && (x < (xBlocks - 1)) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) + 1;					// edge12
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE12);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE10);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (z < (zBlocks - 1)) && (x > 0) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) - 1;					// edge9
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE9);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE11);
				}
				blockStore->releaseBlock(n);
			}
		
		    if ( (z > 0) && (x > 0) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) - 1;					// edge10
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::EDGE10);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::EDGE12);
				}
				blockStore->releaseBlock(n);
			}
			if ( (z > 0) && (x > 0) && (y > 0) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) - xBlocks - 1;					// corner1
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER7);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER1);
				}
				blockStore->releaseBlock(n);
			}
			
			if ( (z > 0) && (x < (xBlocks - 1)) && (y > 0) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) - xBlocks + 1;					// corner2
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER8);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER2);
				}
				blockStore->releaseBlock(n);
			}
			
			if ( (z < (zBlocks - 1)) && (x < (xBlocks - 1)) && (y > 0) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) - xBlocks + 1;					// corner3
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER5);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER3);
				}
				blockStore->releaseBlock(n);
			}
			
			if ( (z < (zBlocks - 1)) && (x > 0) && (y > 0) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) - xBlocks - 1;					// corner4
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER6);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER4);
				}
				blockStore->releaseBlock(n);
			}
			
			if ( (z > 0) && (x > 0) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) + xBlocks - 1;					// corner5
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER3);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER5);
				}
				blockStore->releaseBlock(n);
			}
			
			if ( (z > 0) && (x < (xBlocks - 1)) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex - (xBlocks * yBlocks) + xBlocks + 1;					// corner6
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER4);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER6);
				}
				blockStore->releaseBlock(n);
			}
			
			if ( (z < (zBlocks - 1)) && (x < (xBlocks - 1)) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) + xBlocks + 1;					// corner7
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER1);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER7);
				}
				blockStore->releaseBlock(n);
			}
			
			if ( (z < (zBlocks - 1)) && (x > 0) && (y < (yBlocks - 1)) )
			{
			    n	= blockIndex + (xBlocks * yBlocks) + xBlocks - 1;					// corner8
			    block2 = (goSignal3D<T>*)(blockStore->getBlock (n));
			    if ((!blockStore->fail()) && (!_pm->isScheduled(n))) // Block exists
				{
				    block2->interpolateFromSignal (*block, goSignal3D<T>::CORNER2);
				    block->interpolateFromSignal (*block2, goSignal3D<T>::CORNER8);
				}
				blockStore->releaseBlock(n);
			}
			blockStore->releaseBlock (blockIndex);
		}
	}
}				

template<class T>
bool
goVolumeFile<T>::readTransBlock_bandWise (goSize_t blockNumber, goSignal3D<T>* signal, int stage)
{
    // Info header addieren
    streampos p = (streampos)(0);
    // Startposition des ersten (TP) Bandes
  goSize_t blocks = info.blocks.x * info.blocks.y * info.blocks.z;
  streampos bandLen = (streampos)((info.blockSize.x >> info.stages) * (info.blockSize.y >> info.stages) * (info.blockSize.z >> info.stages)) * sizeof(T);
  // p += bandLen * blockNumber;
  
  goSubSignal3D<T> subSignal;
  subSignal.setParent(signal);
  subSignal.setSize(signal->getSizeX() >> stage, signal->getSizeY() >> stage, signal->getSizeZ() >> stage);
  subSignal.setDiff(signal->getXDiff() << stage, signal->getYDiff() << stage, signal->getZDiff() << stage);
  
  transFile.seekg(p + (streampos)(bandLen * blockNumber));
  /* Read the lowest stage */
  subSignal.setPosition (0,0,0);
  subSignal.read (transFile);
  int i;
  int pos;

  // cout << "info header size = " << GO_VOLUMEFILEINFO_SIZE << endl;
  // cout << "bandLen = " << bandLen << endl;

  p = (streampos) (bandLen * blocks);
  /* Read all detail bands up to stage*/
//    readTransHP (subSignal, (streampos)(bandLen * 7), 
//  	       (int)stage, transFile, 
//  	       p, blocks);
  
  /* Rekursiv machen, das hier ist zu unuebersichtlich! */
  pos = 1 << (stage - 1);
  for (i = 0; i < stage; i++)
  {
    transFile.seekg(p + (streampos)(bandLen * 7 * blockNumber));
    p += bandLen * blocks * 7;
    bandLen = bandLen << 3;
    subSignal.setPosition (pos,0,0);
    subSignal.read (transFile);
    subSignal.setPosition (0,pos,0);
    subSignal.read (transFile);
    subSignal.setPosition (pos,pos,0);
    subSignal.read (transFile);
    subSignal.setPosition (0,0,pos);
    subSignal.read (transFile);
    subSignal.setPosition (pos,0,pos);
    subSignal.read (transFile);
    subSignal.setPosition (0,pos,pos);
    subSignal.read (transFile);
    subSignal.setPosition (pos,pos,pos);
    subSignal.read (transFile);

    subSignal.shiftRightDiff (1);
    subSignal.shiftLeftSize (1);
    pos >>= 1;
  }
  
  goDWT<T> dwt;
  dwt.unHaar (*signal, stage);
  ((goPresenceManager<T>*)pm)->set (blockNumber);
  return true;
}


template<class T>
bool
goVolumeFile<T>::readTransBlock_blockWise (goSize_t blockNumber, goSignal3D<T>* signal, int stage)
{
    // goSize_t bsize = getFileInfo().blockSize.x * getFileInfo().blockSize.y * getFileInfo().blockSize.z;    
    positionAt (blockNumber, stage);
    goDWT<T> dwt;
    blockwise_read (*signal, transFile, 0, stage, true);
    dwt.unHaar (*signal, stage);
    ((goPresenceManager<T>*)pm)->set (blockNumber);
    return true;
}

/*
 * File pointer must be positioned at the beginning of the first band to be read!
 */
template<class T>
void
goVolumeFile<T>::blockwise_read (goSignal3D<T>& signal, ifstream& f, int stage, int stages, bool readLast)
{
    if (stage == stages)
	{
	    if (readLast)
			signal.read (f);
	    return;
	}
    goSubSignal3D<T> subBlock;
    subBlock.setParent (&signal);
    subBlock.setDiff (signal.getXDiff() << 1, signal.getYDiff() << 1, signal.getZDiff() << 1);
    subBlock.setSize (signal.getSizeX() >> 1, signal.getSizeY() >> 1, signal.getSizeZ() >> 1);

    subBlock.setPosition (1, 0, 0);
    subBlock.read (f);
    subBlock.setPosition (0, 1, 0);
    subBlock.read (f);
    subBlock.setPosition (1, 1, 0);
    subBlock.read (f);
    subBlock.setPosition (0, 0, 1);
    subBlock.read (f);
    subBlock.setPosition (1, 0, 1);
    subBlock.read (f);
    subBlock.setPosition (0, 1, 1);
    subBlock.read (f);
    subBlock.setPosition (1, 1, 1);
    subBlock.read (f);
    subBlock.setPosition (0, 0, 0);
    blockwise_read (subBlock, f, stage + 1, stages, readLast);
}


/*
 * Wird dieser Prozess unterbrochen, sind uU die size/diff Werte einiger
 * Blöcke kleiner als vorgesehen. Nicht schlimm, da beim delete Operator 
 * der ganze Block gelöscht wird, sollte aber trotzdem erwähnt werden.
 */
template<class T>
bool
goVolumeFile<T>::readTransBlocks_bandWise()
{
	goTimerObject timer;
#ifdef GO_BENCHMARK
	goDouble	totalSeconds = 0.0f;
#endif
	goSize_t nBlocks = 0;  // only for timing
	
#ifdef MULTIPLE_THREADS
	// I should be in a separate thread, so run forever.
    while (true)
#endif
	{
#ifdef MULTIPLE_THREADS
	    // Wait for semaphore
	    loadSema.dec();
#endif
		timer.startTimer();
	    // Just for safety, this should never be true here.
	    if (!blockQ.isEmpty())
		{
			// First, keep track of which blocks we want to load and up to which stage
			// each block is actually loaded.
			goArray<goSize_t> reqIndices;
			goArray<int>	  reqStages;
			goArray<int>	  reqMinStages;
			goArray<void*>	  reqBlockPtrs;
			QMutex.lock();
			nBlocks = blockQ.getSize();
			reqIndices.resize(blockQ.getSize());
			reqStages.resize(blockQ.getSize());
			reqMinStages.resize(blockQ.getSize());
			reqBlockPtrs.resize(blockQ.getSize());
			goSize_t i = 0;
		    while (!blockQ.isEmpty())
			{
				reqIndices[i] = blockQ.getHead(); blockQ.remove();
				reqStages[i]  = stagesQ.getHead(); stagesQ.remove();
				reqMinStages[i] = minStagesQ.getHead(); minStagesQ.remove();
				reqBlockPtrs[i] = blockPtrQ.getHead(); blockPtrQ.remove();
				i++;
			}
			// Queues can be filled again by another thread now.
			QMutex.unlock();
#if 0	
			for (i = 0; i < reqIndices.getSize(); i++)
			{
				readTransBlock_bandWise(reqIndices[i],(goSignal3D<T>*)reqBlockPtrs[i],reqStages[i]);
			}
			for (i = 0; i < reqIndices.getSize(); i++)
	 		{
				((goPresenceManager<T>*)pm)->set (reqIndices[i]);
			}
			signalProduction(); // only once to start the renderer; everything should be loaded now.
#endif		
#if 1
			// positionAt(0,0);;
			// Alle Blöcke durchgehen, Grössen und Diffs initialisieren.
			for (i = 0; i < (goSize_t)reqIndices.getSize(); i++)
			{
				int shift;
				if (reqStages[i] == 0)
					shift = 0;
				else 
				{
					if (reqMinStages[i] == 0)
						shift = reqStages[i]; // - reqMinStages[i];
					else
						shift = reqStages[i] - reqMinStages[i] + 1;
				}
				((goSignal3D<T>*)reqBlockPtrs[i])->shiftLeftDiff(shift);
				((goSignal3D<T>*)reqBlockPtrs[i])->shiftRightSize(shift);
			}
			goArray<int> loadedStages;
			// Enthält die letzte Stufe für jeden der Blöcke, die schon geladen ist.
			loadedStages.resize(reqIndices.getSize());
			loadedStages.fill(-1);
			bool firstBlock = true;
			goSize_t lastBlockRead = 0;
			// Bandweise durch die Datei laufen, Band 0 (LLL) zuerst:
			for (i = 0; i < (goSize_t)reqIndices.getSize(); i++)
			{
				if (reqMinStages[i] == 0)
				{
					if (firstBlock)
					{
						positionAt(reqIndices[i],0);
						firstBlock = false;
					} else
					{
						int blockDiff = reqIndices[i] - lastBlockRead - 1;
						// positionAt(reqIndices[i],0);
						if (blockDiff != 0)
							move_bandWise(blockDiff,0);
					}
					((goSignal3D<T>*)reqBlockPtrs[i])->read(transFile,false);
					lastBlockRead = reqIndices[i];
					loadedStages[i] = 0;
				}
			}
			int currentBand;
			goDWT<T> dwt;
			// Jetzt die Hochpassbänder:
			for (currentBand = 1; currentBand <= info.stages; currentBand++)
			{
				firstBlock = true;
				lastBlockRead = 0;
				for (i = 0; i < (goSize_t)reqIndices.getSize(); i++)
				{
					// if band currentBand needs to be loaded for block i, do so
					goSignal3D<T>	*block = 0;
					goSubSignal3D<T> subBlock;
					if ( (reqMinStages[i] <= currentBand) &&
						 (reqStages[i] >= currentBand) )
					{
#if _GODEBUG >= 5
						cout << "Block " << reqIndices[i] << " band " << currentBand << ":\n";
#endif
						if (firstBlock)
						{
							positionAt(reqIndices[i],currentBand);
							firstBlock = false;
						} else
						{
							int blockDiff = reqIndices[i] - lastBlockRead - 1;
							// positionAt(reqIndices[i],currentBand);
							if (blockDiff != 0)
								move_bandWise(blockDiff,currentBand);
						}
						lastBlockRead = reqIndices[i];
						block = (goSignal3D<T>*)reqBlockPtrs[i];
						subBlock.setSize(block->getSizeX(),block->getSizeY(),block->getSizeZ());
						subBlock.setDiff(block->getXDiff(),block->getYDiff(),block->getZDiff());
						// Erhöhe die Grössenangabe des eigentlichen Blocks
						block->shiftLeftSize(1);
						block->shiftRightDiff(1);
						// Übernimm kleinere Grösse und grössere Diffs in subBlock
						subBlock.setParent(block);
#if _GODEBUG >= 5
						cout << "\tblocksize = " << block->getSizeX() << "," << block->getSizeY() << "," << block->getSizeZ() << "\n";
						cout << "\tsubblock size = " << subBlock.getSizeX() << "," << subBlock.getSizeY() << "," << subBlock.getSizeZ() << "\n";
#endif
						// Lade die 7 Hochpassanteile
						subBlock.setPosition(1,0,0);
						subBlock.read(transFile,false);
						subBlock.setPosition(0,1,0);
						subBlock.read(transFile,false);
						subBlock.setPosition(1,1,0);
						subBlock.read(transFile,false);
						subBlock.setPosition(0,0,1);
						subBlock.read(transFile,false);
						subBlock.setPosition(1,0,1);
						subBlock.read(transFile,false);
						subBlock.setPosition(0,1,1);
						subBlock.read(transFile,false);
						subBlock.setPosition(1,1,1);
						subBlock.read(transFile,false);
						dwt.unHaar(*block,1);	
					}
				}
			}
			// Alle present setzen. Ändern, so dass "hurry" Signal benutzt werden kann.		
			for (i = 0; i < (goSize_t)reqIndices.getSize(); i++)
	 		{
				// dwt.unHaar(*((goSignal3D<T>*)reqBlockPtrs[i]),reqStages[i]);
				((goPresenceManager<T>*)pm)->set (reqIndices[i]);
			    interpolationMutex.lock();
			    interpolationQ.add (reqIndices[i]);
			    interpolationMutex.unlock();
			    interpolationSema.inc(); // increment the semaphore the interpolation thread is waiting for
			}
			signalProduction(); // only once to start the renderer; everything should be loaded now.
#endif
		}
		timer.stopTimer();
		// NEU wird nur zum benchmarking gebraucht!
		/***************************/
		setProgress(1.0f);
		signalProgress();
		/***************************/
#ifdef GO_BENCHMARK
		totalSeconds += timer.getTimerSeconds();
		cout << getClassName() << " Benchmark: total load time " << totalSeconds << "\n";
#endif
#if 	_GODEBUG >= 1
		cout << getClassName() << ": Time for loading " << nBlocks << " blocks _bandwise_: " << timer.getTimerSeconds() << "s\n";
#endif
	}
	return true;
#if 0
    goIndex_t i;
    goSignal3D<T>* signal;
    int stage;
    
    /*
     * Keine Beschleunigung
     */
    for (i = 0; i < blockNumbers.getSize(); i++)
	{
	    signal = (goSignal3D<T>*)signalPointers[i];
	    stage  = stages[i];
	    readTransBlock_bandWise (blockNumbers[i], signal, stage);
	    signalProduction();	// One block is loaded, so wake up any interested thread (goConsumer).
	}
    return true;
#endif
}

// blockNumbers muss monoton steigend sein, alle Blocknummern muessen paarweise verschieden sein.
// DWT/laden multithreading!!!
template<class T>
bool
goVolumeFile<T>::readTransBlocks_blockWise ()
{
    goSize_t i, i_next;
    goDWT<T> dwt;
    int maxStage = getFileInfo().stages;
    int minStage = 0;
    int stage = 0, stage_next = 0;
    bool readLast;
    goSignal3D<T>* signalPtr = 0;
	goTimerObject timer;
#ifdef GO_BENCHMARK
	goDouble 	  totalSeconds = 0.0f;
#endif	
	goSize_t nBlocks = 0;  // only for the timing
#ifdef MULTIPLE_THREADS
    while (true)
#endif
	{
#ifdef MULTIPLE_THREADS
	    // Wait for semaphore
	    loadSema.dec();
#endif
		timer.startTimer();
	    // Just for safety, this should never be true here.
	    if (!blockQ.isEmpty())
		{
		    QMutex.lock();
			nBlocks = blockQ.getSize();
		    i = blockQ.getHead();
		    stage = stagesQ.getHead();
		    minStage = minStagesQ.getHead();
		    signalPtr = (goSignal3D<T>*)blockPtrQ.getHead();
		    positionAt (i, stage); // Position at first block
#if _GODEBUG >= 1
			cout << getClassName() << ": blockwise read: Q size is " << blockQ.getSize() << endl;
#endif			
		    while (blockQ.getSize() > 1)
			{
			    //cout << getClassName() << ": blockwise read thread reading block " << i << endl;
			    
			    blockQ.remove();
			    stagesQ.remove();
			    minStagesQ.remove();
			    blockPtrQ.remove();
			    i_next = blockQ.getHead();
			    stage_next = stagesQ.getHead();
			   
#if _GODEBUG >= 4
			   	cout << getClassName() << ": reading block " << i << endl;
				cout << "\tsignal size: " << signalPtr->getSizeX()  << "," << signalPtr->getSizeX() << "," << signalPtr->getSizeZ() << endl;
				cout << "\tsignal stage from " << minStage << " to " << stage << endl;
#endif
			    readLast = (minStage == 0);
			    minStage = readLast ? 0 : minStage - 1;
			    blockwise_read (*signalPtr, transFile, minStage, stage, readLast); // Read one block
			
			    if (readLast && (i_next > i))
				{
//					cout << "\tMoving for " << i_next - i - 1 << " blocks and " << maxStage - stage_next << " stages forward" << endl;
					move(i_next - i - 1, maxStage - stage_next); // Move to the next block
				}
			    else
				{
//					cout << "\tPositioning at " << i_next << ", stage " << stage_next << endl;
					positionAt (i_next, stage_next);
				}
			
			    dwt.unHaar (*signalPtr, stage - minStage);
			
			    //	    ((goSignal3D<T>*)signalPointers[i])->interpolateBorders();
			
			    ((goPresenceManager<T>*)pm)->set (i);
			    interpolationMutex.lock();
			    interpolationQ.add (i);
			    interpolationMutex.unlock();
#ifdef MULTIPLE_THREADS
			    interpolationSema.inc(); // increment the semaphore the interpolation thread is waiting for
#endif
			    signalProduction();	// One block is loaded, so wake up any interested thread (goConsumer).
		
			    i = i_next;
			    stage = stage_next;
			    signalPtr = (goSignal3D<T>*)blockPtrQ.getHead();
			    minStage  = minStagesQ.getHead();
			
			    // readTransBlock_blockWise (blockNumbers[i], (goSignal3D<T>*)signalPointers[i], stages[i]);
			    // cout << "calling signalProduction() for the " << i + 1 << "th time" << endl;
			} 
#if _GODEBUG >= 4
			cout << getClassName() << ": reading block " << i << endl;
			cout << "\tsignal size: " << signalPtr->getSizeX()  << "," << signalPtr->getSizeX() << "," << signalPtr->getSizeZ() << endl;
			cout << "\tsignal stage from " << minStage << " to " << stage << endl;
			cout << "\tBlock is last in queue" << endl;
			cout << "\tsignalPtr  = " << signalPtr << endl;
			cout << "\tQueue size = " << blockPtrQ.getSize() << endl;
#endif			
		    readLast = (minStage == 0);
		    minStage = readLast ? 0 : minStage - 1;
		    blockwise_read (*signalPtr, transFile, minStage, stage, readLast); // Read last block
		    dwt.unHaar (*signalPtr, stage - minStage);
			
		    blockQ.remove();
		    stagesQ.remove();
		    minStagesQ.remove();
		    blockPtrQ.remove();
#if _GODEBUG >= 4
			cout << "\t...read and removed from queue." << endl;
#endif			
		    //  ((goSignal3D<T>*)signalPointers[i])->interpolateBorders();
			
		    ((goPresenceManager<T>*)pm)->set (i);
		    signalProduction (); // And signal it's production
		    interpolationMutex.lock();
		    interpolationQ.add (i);
		    interpolationMutex.unlock();
		    interpolationSema.inc(); // increment the semaphore the interpolation thread is waiting for
			
		    QMutex.unlock();
		}
		timer.stopTimer();
#ifdef GO_BENCHMARK
		totalSeconds += timer.getTimerSeconds();
		cout << getClassName() << " Benchmark: total load time " << totalSeconds << "\n";
#endif
#if	_GODEBUG >= 1
		cout << getClassName() << ": Time for loading " << nBlocks << " blocks _blockwise_: " << timer.getTimerSeconds() << "s\n";
#endif
		// NEU wird nur zum benchmarking gebraucht!
		/***************************/
		setProgress(1.0f);
		signalProgress();
		/***************************/
	}
#ifndef MULTIPLE_THREADS
	interpolateBlocks();
#endif
    this->setBusy (false);	// Used from a thread, set busy to false when done.
    return true;
}


template<class T>
bool
goVolumeFile<T>::linear2Trans_bandWise (goSize3D& blockSize, int stage, bool block_by_block, bool slow_loading)
{
    /* To help find out the correct dimensions */
    goCluster3D<goInt8> cluster;
    goSignal3D<T> signal;
    goDWT<T>	dwt;

    info.blockSize = blockSize;
    info.stages = stage;

    cluster.setSize(info.size.x, info.size.y, info.size.z);
    cluster.setBlockSize (blockSize.x, blockSize.y, blockSize.z);
    cluster.update();
    info.blocks.x = cluster.getXBlocks();
    info.blocks.y = cluster.getYBlocks();
    info.blocks.z = cluster.getZBlocks();
    signal.make (cluster.getXBlocks() * blockSize.x, cluster.getYBlocks() * blockSize.y,
		 blockSize.z, 0, 0, 0);
    goSize_t slice = 0;
    goSize_t slice2 = slice + blockSize.z;
    goSize_t slab;
    goSubSignal3D<T>	block;
    block.setParent (&signal);
    block.setSize(blockSize.x, blockSize.y, blockSize.z);
    block.setDiff(signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
    goSize_t posX, posY;

  
    goSize_t x, y, z;
    x = cluster.getXBlocks();
    y = cluster.getYBlocks();
    z = cluster.getZBlocks();
  
    // ofstream *bandFiles = new ofstream*[stage + 1];
	goArray<void*> bandFiles;  // ofstream*
	bandFiles.resize(stage+1);
    goString *bandFileNames = new goString[stage + 1];
    int i;
    /*
     * Open temporary files
     */
    for (i = 0; i < (stage + 1); i++)
	{
	    bandFileNames[i] = "TEMPBAND_XXXXXX";
		bandFiles[i] = (void*)(new ofstream(mkstemp(bandFileNames[i].getPtr())));
	    // tmpnam((char*)bandFileNames[i].getPtr());
	    // bandFiles[i].open(bandFileNames[i].toCharPtr(), ios::out|ios::binary);
	    if (((ofstream*)bandFiles[i])->fail())
		{
		    goString s;
		    s = "Could not open temporary file ";
		    s += bandFileNames[i].toCharPtr();
		    goError::print("goVolumeFile::linear2Trans()",s);
		}
	    else
		{
		    goString s;
		    s = "Opened temporary file ";
		    s += bandFileNames[i].toCharPtr();
		    goError::note("goVolumeFile::linear2Trans()",s);
		}
	}
/* ---------------------------------------------------------------------------- */  
	goDouble 		  mean = 0;
	goDouble 		  energy = 0;
	goDouble 		  blocks_1 = 1 / (float)(x * y * z);
	goDouble		  minimum = 0;
	goDouble		  maximum = 0;
	bool			  firstRun;     // Have to use this, since g++ 2.95 does not have numeric_limits
	firstRun = true;
	goSignalStat<T>   stats;
	// Linewise conversion
	if (block_by_block)
	{
		// Make one line of blocks
		signal.make(info.size.x, info.blockSize.y, info.blockSize.z, info.blockSize.x, 0, 0);
		signal.fill(0);
		goSubSignal3D<T> block;
		block.setParent(&signal);
		block.setSize(info.blockSize.x, info.blockSize.y, info.blockSize.z);
		block.setDiff(signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
		goSize_t posZ, posY, posX;
		posZ = 0;
		for (z = 0; z < info.blocks.z; z++)
		{
			posY = 0;
			for (y = 0; y < info.blocks.y; y++)
			{
				// Read one line of blocks 
			        readLinear(signal, 0, posY, posZ, slow_loading);
				goSize_t bi; // blockindex
				posX = 0;
				for (bi = 0; bi < info.blocks.x; bi++)
				{
					T temp;
					block.setPosition(posX,0,0);
					if (firstRun)
					{
						minimum = block.getMinimum();
						maximum = block.getMaximum();
						firstRun = false;
				    }
					temp = block.getMinimum();
					if (temp < minimum)
					{
					    minimum = temp;
					}
					temp = block.getMaximum();
					if (temp > maximum)
					{
					    maximum = temp;
					}
					mean      += stats.getMean (block) * blocks_1;
					energy    += stats.getEnergy(block);
					dwt.haar(block, stage);
					/*
					 * Blockwise, band by band write:
					 */ 
			    	writeBands (block, stage, bandFiles);
		            // block.write(outfile);
					posX += info.blockSize.x;
				}
				posY += info.blockSize.y;
			}
			posZ += info.blockSize.z;
		}
		signal.destroy();
	} else
	{
	    for (slab = 0; slab < cluster.getZBlocks(); slab++)
		{
		    readLinear (signal, slice, slice2, slow_loading);
		    slice  += blockSize.z;
		    slice2 += blockSize.z;
		    for (posY = 0; posY < signal.getSizeY(); posY += blockSize.y)
			{
			    for (posX = 0; posX < signal.getSizeX(); posX += blockSize.x)
				{
					T temp;
					block.setPosition (posX, posY, 0);
					if (firstRun)
					    {
							minimum = block.getMinimum();
							maximum = block.getMaximum();
							firstRun = false;
					    }
					temp = block.getMinimum();
					if (temp < minimum)
					{
					    minimum = temp;
					}
					temp = block.getMaximum();
					if (temp > maximum)
					{
					    maximum = temp;
					}
					mean      += stats.getMean (block) * blocks_1;
					energy    += stats.getEnergy(block);
	
				    dwt.haar(block, stage);
				    writeBands (block, stage, bandFiles);
				    // block.write(outfile);
				}
			}
		}
	}
	/* ------------------------------------------------------------------------ */
    for (i = 0; i < (stage + 1); i++)
	{
	    ((ofstream*)bandFiles[i])->close();
	}

    ofstream outfile;
    outfile.open (transFileName.toCharPtr(), ios::out|ios::binary);
    if (outfile.fail())
	{
	    goString s;
	    s = "Cannot open transform file ";
	    s += transFileName.toCharPtr();
	    s += " for writing";
	    goError::print("goVolumeFile::linear2Trans()",s);
	    return false;
	}
    cout << "Calling concatBands" << endl;
    concatBands (outfile, bandFileNames, stage);
	info.version = 0;
    // Statistik
    info.energy  = energy;
    info.mean    = mean;
    info.minimum = minimum;
    info.maximum = maximum;
	if (transFileType == GO_VOLUMEFILE_BLOCKWISE)
		info.fileType = 0;
	else info.fileType = 1;	
    writeFileInfo (info, outfile);
    outfile.close ();
  
    for (i = 0; i < (stage + 1); i++)
	{
	    if (::remove(bandFileNames[i].toCharPtr()) == -1)
		{
		    goString s;
		    s = "Cannot remove file ";
		    s += bandFileNames[i];
		    goError::print("goVolumeFile::linear2Trans()",s);
		}
	}

    signal.destroy();
	
    delete[] bandFileNames;
	for (i = 0; i < bandFiles.getSize(); i++)
		delete (ofstream*)bandFiles[i];	
    return true;
}


template<class T>
bool
goVolumeFile<T>::l2t_blockwise_init (goSize3D& blockSize, int stage, ofstream* outfile)
{
    /* To help find out the correct dimensions */
    goCluster3D<goInt8> cluster;
    
    info.blockSize  = blockSize;
    info.stages 	= stage;
    
    cluster.setSize(info.size.x, info.size.y, info.size.z);
    cluster.setBlockSize (blockSize.x, blockSize.y, blockSize.z);
    cluster.update();
    info.blocks.x = cluster.getXBlocks();
    info.blocks.y = cluster.getYBlocks();
    info.blocks.z = cluster.getZBlocks();

    //    signal.make (cluster.getXBlocks() * blockSize.x, cluster.getYBlocks() * blockSize.y,
    //		 blockSize.z, 0, 0, 0);
    outfile->open (transFileName.toCharPtr(), ios::out|ios::binary);
	goSignal3D<T> dummySignal;
	// reset
	l2t_blockwise_writeslab(dummySignal, outfile, true);
	l2t_blockwise_writeblock(dummySignal, outfile, true);
    return (!outfile->fail());
}

template<class T>
bool
goVolumeFile<T>::l2t_bandwise_init (goSize3D& blockSize, int stage)
{
    /* To help find out the correct dimensions */
    goCluster3D<goInt8> cluster;

    info.blockSize = blockSize;
    info.stages = stage;

    cluster.setSize(info.size.x, info.size.y, info.size.z);
    cluster.setBlockSize (blockSize.x, blockSize.y, blockSize.z);
    cluster.update();
    info.blocks.x = cluster.getXBlocks();
    info.blocks.y = cluster.getYBlocks();
    info.blocks.z = cluster.getZBlocks();
  
    int i;
	// Nur zur Sicherheit...
 	for(i = 0; i < bandwise_bandFiles.getSize(); i++) 
	{
		delete (ofstream*)bandwise_bandFiles[i];
	}
	bandwise_bandFiles.resize(stage + 1);
	if (bandwise_bandFileNames)
	{
		delete bandwise_bandFileNames;
	}
    bandwise_bandFileNames = new goString[stage + 1];
    /*
     * Open temporary files
     */
    for (i = 0; i < (stage + 1); i++)
	{
	    // bandwise_bandFileNames[i].resize(L_tmpnam + 1);
	    // tmpnam((char*)bandwise_bandFileNames[i].getPtr());
	    // bandwise_bandFiles[i].open(bandwise_bandFileNames[i].toCharPtr(), ios::out|ios::binary);
		bandwise_bandFileNames[i] = "TEMPBANDS_XXXXXX";
		bandwise_bandFiles[i] = (void*)(new ofstream(mkstemp(bandwise_bandFileNames[i].getPtr())));
	    if (((ofstream*)bandwise_bandFiles[i])->fail())
		{
		    goString s;
		    s = "Could not open temporary file ";
		    s += bandwise_bandFileNames[i].toCharPtr();
		    goError::print("goVolumeFile::linear2Trans()",s);
			return false;
		}
	    else
		{
		    goString s;
		    s = "Opened temporary file ";
		    s += bandwise_bandFileNames[i].toCharPtr();
		    goError::note("goVolumeFile::linear2Trans()",s);
		}
	}
	goSignal3D<T> dummySignal;
	l2t_bandwise_writeslab(dummySignal, true);
	// l2t_bandwise_writeblock(dummySignal, true);
	return true;
}

template<class T>
bool
goVolumeFile<T>::l2t_blockwise_writeslab (goSignal3D<T>& signal, ofstream* outfile, bool reset)
{
    goSubSignal3D<T>	block;
    block.setParent (&signal);
    block.setSize(info.blockSize.x, info.blockSize.y, info.blockSize.z);
    block.setDiff(signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
    goSize_t posX, posY;
    goDWT<T>		dwt;

    goSize_t x, y, z;
    x = info.blocks.x;
    y = info.blocks.y;
    z = info.blocks.z;
  
    static goDouble 		  mean = 0;
    static goDouble 		  energy = 0;
    static goDouble 		  blocks_1 = 1 / (float)(x * y * z);
    static goDouble		  minimum = 0;
    static goDouble		  maximum = 0;
    static bool			  firstRun = true;     // Have to use this, since g++ 2.95 does not have numeric_limits
	if (reset)
	{
		firstRun = true; 
		return true;
	}
    static goSignalStat<T>   stats;
    for (posY = 0; posY < signal.getSizeY(); posY += info.blockSize.y)
	{
	    for (posX = 0; posX < signal.getSizeX(); posX += info.blockSize.x)
		{
		    T temp;
		    block.setPosition (posX, posY, 0);
		    if (firstRun)
			{
			    minimum = block.getMinimum();
			    maximum = block.getMaximum();
			    firstRun = false;
			}
		    temp = block.getMinimum();
		    if (temp < minimum)
			{
			    minimum = temp;
			}
		    temp = block.getMaximum();
		    if (temp > maximum)
			{
			    maximum = temp;
			}
		    mean      += stats.getMean (block) * blocks_1;
		    energy    += stats.getEnergy(block);
		    dwt.haar(block, info.stages);
		    /*
		     * Blockwise, band by band write:
		     */ 
		    l2t_blockwise_write(block, *outfile, 0, info.stages);
		    // block.write(outfile);
		}
	}
    // Statistik
    info.energy  = energy;
    info.mean    = mean;
    info.minimum = minimum;
    info.maximum = maximum;
    return true;
}

template<class T>
bool
goVolumeFile<T>::l2t_blockwise_writeblock (goSignal3D<T>& block, ofstream* outfile, bool reset)
{
    goDWT<T> dwt;

    static goDouble 		  mean = 0;
    static goDouble 		  energy = 0;
    static goDouble		  minimum = 0;
    static goDouble		  maximum = 0;
    static bool firstRun = true;     // Have to use this, since g++ 2.95 does not have numeric_limits
	if (reset)
	{
		firstRun = true;
		return true;
	}
    static goSignalStat<T>   stats;
    T temp;
    if (firstRun)
	{
	    minimum = block.getMinimum();
	    maximum = block.getMaximum();
	    firstRun = false;
	}
    temp = block.getMinimum();
    if (temp < minimum)
	{
	    minimum = temp;
	}
    temp = block.getMaximum();
    if (temp > maximum)
	{
	    maximum = temp;
	}
    mean      += stats.getMean (block) / (float)(info.blocks.x * info.blocks.y * info.blocks.z);
    energy    += stats.getEnergy(block);
    dwt.haar(block, info.stages);
    /*
     * Blockwise, band by band write:
     */ 
    l2t_blockwise_write(block, *outfile, 0, info.stages);
    // Statistik
    info.energy  = energy;
    info.mean    = mean;
    info.minimum = minimum;
    info.maximum = maximum;
    return true;
}

template<class T>
bool
goVolumeFile<T>::l2t_bandwise_writeslab (goSignal3D<T>& signal, bool reset)
{
    goSubSignal3D<T>	block;
    block.setParent (&signal);
    block.setSize(info.blockSize.x, info.blockSize.y, info.blockSize.z);
    block.setDiff(signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
    goSize_t posX, posY;
    goDWT<T>		dwt;

    goSize_t x, y, z;
    x = info.blocks.x;
    y = info.blocks.y;
    z = info.blocks.z;
	
	static goDouble 	mean = 0;
	static goDouble 	energy = 0;
	static goDouble 	blocks_1 = 1 / (float)(x * y * z);
	static goDouble		minimum = 0;
	static goDouble		maximum = 0;
	static bool			firstRun = true;     // Have to use this, since g++ 2.95 does not have numeric_limits
	if(reset)
	{
		firstRun = true;
		return true;
	}
	static goSignalStat<T>   stats;
	for (posY = 0; posY < signal.getSizeY(); posY += info.blockSize.y)
	{
	    for (posX = 0; posX < signal.getSizeX(); posX += info.blockSize.x)
		{
			T temp;
			block.setPosition (posX, posY, 0);
			if (firstRun)
			    {
					minimum = block.getMinimum();
					maximum = block.getMaximum();
					firstRun = false;
			    }
			temp = block.getMinimum();
			if (temp < minimum)
			{
			    minimum = temp;
			}
			temp = block.getMaximum();
			if (temp > maximum)
			{
			    maximum = temp;
			}
			mean      += stats.getMean (block) * blocks_1;
			energy    += stats.getEnergy(block);

		    dwt.haar(block, info.stages);
		    writeBands (block, info.stages, bandwise_bandFiles);
		    // block.write(outfile);
		}
	}
	/* Statistik */
    info.energy  = energy;
    info.mean    = mean;
    info.minimum = minimum;
    info.maximum = maximum;
	return true;
}

template<class T>
bool
goVolumeFile<T>::l2t_blockwise_finish (ofstream* outfile)
{
    // Version
    info.version = 0;
	if (transFileType == GO_VOLUMEFILE_BLOCKWISE)
		info.fileType = 0;
	else info.fileType = 1;	
    writeFileInfo (info, *outfile);
    outfile->close ();
    return true;
}

template<class T>
bool
goVolumeFile<T>::l2t_bandwise_finish()
{
	int stage = info.stages;
	int i;
    for (i = 0; i < (stage + 1); i++)
	{
	    ((ofstream*)bandwise_bandFiles[i])->close();
	}

    ofstream outfile;
    outfile.open (transFileName.toCharPtr(), ios::out|ios::binary);
    if (outfile.fail())
	{
	    goString s;
	    s = "Cannot open transform file ";
	    s += transFileName.toCharPtr();
	    s += " for writing";
	    goError::print("goVolumeFile::linear2Trans()",s);
	    return false;
	}
    cout << "Calling concatBands" << endl;
    concatBands (outfile, bandwise_bandFileNames, stage);
	info.version = 0;
    // Statistik
	if (transFileType == GO_VOLUMEFILE_BLOCKWISE)
		info.fileType = 0;
	else info.fileType = 1;	
    writeFileInfo (info, outfile);
    outfile.close ();
  
    for (i = 0; i < (stage + 1); i++)
	{
	    if (::remove(bandwise_bandFileNames[i].toCharPtr()) == -1)
		{
		    goString s;
		    s = "Cannot remove file ";
		    s += bandwise_bandFileNames[i];
		    goError::print("goVolumeFile::linear2Trans()",s);
		}
	}

	for (i = 0; i < bandwise_bandFiles.getSize(); i++)
	{
		delete (ofstream*)bandwise_bandFiles[i];
	}
	bandwise_bandFiles.resize(0);
    delete[] bandwise_bandFileNames;
	bandwise_bandFileNames = 0;
	
    return true;
}

template<class T>
bool
goVolumeFile<T>::linear2Trans_blockWise (goSize3D& blockSize, int stage, bool block_by_block, bool slow_loading)
{
    
    /* To help find out the correct dimensions */
    goCluster3D<goInt8> cluster;
    goSignal3D<T> 	signal;
    goDWT<T>		dwt;
    
    info.blockSize  = blockSize;
    info.stages 	= stage;
    
    cluster.setSize(info.size.x, info.size.y, info.size.z);
    cluster.setBlockSize (blockSize.x, blockSize.y, blockSize.z);
    cluster.update();
    info.blocks.x = cluster.getXBlocks();
    info.blocks.y = cluster.getYBlocks();
    info.blocks.z = cluster.getZBlocks();
	

  
    goSize_t x, y, z;
    x = cluster.getXBlocks();
    y = cluster.getYBlocks();
    z = cluster.getZBlocks();
  
    ofstream outfile;
    outfile.open (transFileName.toCharPtr(), ios::out|ios::binary);
    if (outfile.fail())
	{
	    goString s;
	    s = "Cannot open transform file ";
	    s += transFileName.toCharPtr();
	    s += " for writing";
	    goError::print("goVolumeFile::linear2Trans()",s);
	    return false;
	}
	
	goDouble 		  mean = 0;
	goDouble 		  energy = 0;
	goDouble 		  blocks_1 = 1 / (float)(x * y * z);
	goDouble		  minimum = 0;
	goDouble		  maximum = 0;
	bool			  firstRun;     // Have to use this, since g++ 2.95 does not have numeric_limits
	firstRun = true;
	goSignalStat<T>   stats;
	
	// Linewise conversion
	if (block_by_block)
	{
		// Make one line of blocks
		signal.make(info.size.x, info.blockSize.y, info.blockSize.z, info.blockSize.x, 0, 0);
		signal.fill(0);
		goSubSignal3D<T> block;
		block.setParent(&signal);
		block.setSize(info.blockSize.x, info.blockSize.y, info.blockSize.z);
		block.setDiff(signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
		goSize_t posZ, posY, posX;
		posZ = 0;
		for (z = 0; z < info.blocks.z; z++)
		{
			posY = 0;
			for (y = 0; y < info.blocks.y; y++)
			{
				// Read one line of blocks 
				readLinear(signal, 0, posY, posZ, slow_loading);
				goSize_t bi; // blockindex
				posX = 0;
				for (bi = 0; bi < info.blocks.x; bi++)
				{
					T temp;
					block.setPosition(posX,0,0);
					if (firstRun)
					{
						minimum = block.getMinimum();
						maximum = block.getMaximum();
						firstRun = false;
				    }
					temp = block.getMinimum();
					if (temp < minimum)
					{
					    minimum = temp;
					}
					temp = block.getMaximum();
					if (temp > maximum)
					{
					    maximum = temp;
					}
					mean      += stats.getMean (block) * blocks_1;
					energy    += stats.getEnergy(block);
					dwt.haar(block, stage);
					/*
					 * Blockwise, band by band write:
					 */ 
					l2t_blockwise_write(block, outfile, 0, stage);
		            // block.write(outfile);
					posX += info.blockSize.x;
				}
				posY += info.blockSize.y;
			}
			posZ += info.blockSize.z;
		}

		signal.destroy();
	} else
	{
		// Slabwise conversion
		// Make one slab of data, able to contain one slice of blocks
   	 	// signal.make (cluster.getXBlocks() * blockSize.x, cluster.getYBlocks() * blockSize.y,
		// 			 blockSize.z, 0, 0, 0);
   	 	signal.make (info.size.x, info.size.y,
		 			 blockSize.z, blockSize.x, blockSize.y, 0);
   	 	goSize_t slice  = 0;
    	goSize_t slice2 = slice + blockSize.z;
    	goSize_t slab;
		// Subblock, moved over the slab
    	goSubSignal3D<T>	block;
	  	block.setParent (&signal);
    	block.setSize(blockSize.x, blockSize.y, blockSize.z);
    	block.setDiff(signal.getXDiff(), signal.getYDiff(), signal.getZDiff());
    	goSize_t posX, posY;
		signal.fill(0);
		// Move the subblock over the slab, saving each block in the outfile
    	for (slab = 0; slab < cluster.getZBlocks(); slab++)
		{
	    	readLinear (signal, slice, slice2, slow_loading);
		    slice  += blockSize.z;
		    slice2 += blockSize.z;
		    for (posY = 0; posY < signal.getSizeY(); posY += blockSize.y)
			{
			    for (posX = 0; posX < signal.getSizeX(); posX += blockSize.x)
				{
					T temp;
					block.setPosition (posX, posY, 0);
					if (firstRun)
					    {
						minimum = block.getMinimum();
						maximum = block.getMaximum();
						firstRun = false;
					    }
					temp = block.getMinimum();
					if (temp < minimum)
					{
					    minimum = temp;
					}
					temp = block.getMaximum();
					if (temp > maximum)
					{
					    maximum = temp;
					}
					mean      += stats.getMean (block) * blocks_1;
					energy    += stats.getEnergy(block);
					dwt.haar(block, stage);
					/*
					 * Blockwise, band by band write:
					 */ 
		            l2t_blockwise_write(block, outfile, 0, stage);
		            // block.write(outfile);
				}
			}
		}
	}  // end else
	// Statistik
	info.energy  = energy;
	info.mean    = mean;
	info.minimum = minimum;
	info.maximum = maximum;
	
	// Version
	info.version = 0;
	if (transFileType == GO_VOLUMEFILE_BLOCKWISE)
		info.fileType = 0;
	else info.fileType = 1;	
	writeFileInfo (info, outfile);
	outfile.close ();
	
	signal.destroy();
	
    return true;
}

/*
 * Writes highest detail first, low pass last.
 */
template<class T>
void
goVolumeFile<T>::l2t_blockwise_write (goSignal3D<T>& block, ofstream& f, int stage, int stages)
{
    if (stage == stages)
	{
	    block.write (f,false);
	    return;
	}
    goSubSignal3D<T> subBlock;
    subBlock.setParent (&block);
    subBlock.setDiff (block.getXDiff() << 1, block.getYDiff() << 1, block.getZDiff() << 1);
    subBlock.setSize (block.getSizeX() >> 1, block.getSizeY() >> 1, block.getSizeZ() >> 1);

    subBlock.setPosition (1, 0, 0);
    subBlock.write (f,false);
    subBlock.setPosition (0, 1, 0);
    subBlock.write (f,false);
    subBlock.setPosition (1, 1, 0);
    subBlock.write (f,false);
    subBlock.setPosition (0, 0, 1);
    subBlock.write (f,false);
    subBlock.setPosition (1, 0, 1);
    subBlock.write (f,false);
    subBlock.setPosition (0, 1, 1);
    subBlock.write (f,false);
    subBlock.setPosition (1, 1, 1);
    subBlock.write (f,false);
    subBlock.setPosition (0, 0, 0);
    l2t_blockwise_write (subBlock, f, stage + 1, stages);
}

template<class T>
void
goVolumeFile<T>::positionAt (goSize_t blockNumber, int stage)
{
	if (transFile.fail())
	{
		cout << getClassName() << ": positionAt transFile fail() == true\n";
		cout << "\tPosition: " << transFile.tellg() << "\n";
		if (transFile.eof())
			cout << "\teof\n";
	}
	streampos p;
 	switch (transFileType)
	{
		case GO_VOLUMEFILE_BLOCKWISE:
		{
			// Position stream pointer at start of block [blockNumber], stage [stage]
    		p = (streampos)(blockNumber * info.blockLength);	// Start of highest band
    		int i;
    		for (i = info.stages; i > stage; i--)
			{
			    p += (streampos)(info.stageLengths[i] * sizeof(T));
			}
			break;
		}
		case GO_VOLUMEFILE_BANDWISE:
		{
			// Position stream pointer at start of block [blockNumber], stage [stage]
			int i;
			p = 0;
			for (i = 0; i < stage; i++)
			{
#if _GODEBUG >= 5
				cout << getClassName() << ": Adding to position for stage " << i << ": ";
#endif
				p += (streampos)(info.totalBandLengths[i] * sizeof(T));
#if _GODEBUG >= 5
				cout << "p now " << p << "\n";
#endif
			}
			p += (streampos)(info.stageLengths[stage] * blockNumber * sizeof(T));
#if _GODEBUG >= 5
			cout << "p finally is " << p << endl;
#endif
			break;
		}
		default:
			p = 0; break;
	}
    transFile.seekg (p, ios::beg);
}   

// Needs to know which stage we are currently positioned in
template<class T>
void
goVolumeFile<T>::move_bandWise(goIndex_t blocksDiff, int currentStage)
{
	if (transFile.fail())
	{
		cout << getClassName() << ": move transFile fail() == true\n";
		cout << "\tPosition: " << transFile.tellg() << "\n";
		if (transFile.eof())
			cout << "\teof\n";
	}
	streamoff dp = (streamoff)(blocksDiff * info.stageLengths[currentStage] * sizeof(T)); 
#if _GODEBUG >= 5	
	cout << getClassName() << ": moving for " << dp << " bytes\n";
#endif	
	transFile.seekg(dp, ios::cur);
}

template<class T>
void
goVolumeFile<T>::move(goIndex_t blocksDiff)
{
    streamoff dp = (streamoff)(blocksDiff * info.blockLength);
    transFile.seekg (dp, ios::cur);
}

template<class T>
void
goVolumeFile<T>::move(goIndex_t blocksDiff, int skipStages)
{
    streamoff dp = (streamoff)(blocksDiff * info.blockLength);
    goIndex_t i = info.stages;
    int i2;
    for (i2 = 0; i2 < skipStages; i2++)
	{
	    dp += (streamoff) (info.stageLengths[i] * sizeof(T));
		--i;
	}
    if (dp != 0) 
	transFile.seekg (dp, ios::cur);
}

}

template class Vol::goVolumeFile<goInt8>;
template class Vol::goVolumeFile<goUInt8>;
template class Vol::goVolumeFile<goInt16>;
template class Vol::goVolumeFile<goUInt16>;
template class Vol::goVolumeFile<goInt32>;
template class Vol::goVolumeFile<goUInt32>;
template class Vol::goVolumeFile<goFloat>;
template class Vol::goVolumeFile<goDouble>;
