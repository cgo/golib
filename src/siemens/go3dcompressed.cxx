#include <config.h>
#include <go3dcompressed.h>
#include <go3dcodersubband.h>
#include <goarray.h>
#include <goerror.h>
#include <godefs.h>
#include <gostring.h>

#include <string.h> // memset()

#include <go3dmacros.h>

template< class T, class CODE_T >
go3DCompressed<T,CODE_T>::go3DCompressed () {
  coder = 0;
  go3DSize_t s;
  s.x = 16;
  s.y = 16;
  s.z = 16;
  setBlockSize (s);
  setStages (1);
  transformTime = 0;
  coderTime = 0;

  indexArray.resize(0);
}

template< class T, class CODE_T >
go3DCompressed<T,CODE_T>::~go3DCompressed () {
  destroyCodedStreams();
  trans.destroy();
  returnBlock.destroy();
  indexArray.resize(0);
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
compress (go3DBlock<T> *volume) {
  go3DSubBlock<T> subblock;
  subblock.setSize (blockSize.x,blockSize.y,blockSize.z);

  goPtrdiff_t dx = blockSize.x * volume->getXDiff();
  goPtrdiff_t dy = blockSize.y * volume->getYDiff();
  goPtrdiff_t dz = blockSize.z * volume->getZDiff();

  goSize_t i,j,k;
  
  /* Assume that this does not give any remainder (for now). */
  goSize_t x = (goSize_t) (volume->getSizeX() / (float)blockSize.x);
  goSize_t y = (goSize_t) (volume->getSizeY() / (float)blockSize.y);
  goSize_t z = (goSize_t) (volume->getSizeZ() / (float)blockSize.z);
  subblock.setParent (volume);
  subblock.setPosition (0,0,0);

  T *p = subblock.getPtr();
  T *pSave = p;
  T *pSave2 = p;

  destroyCodedStreams();

  /* 
   * The position member of subblock is invalid when go3DSubBlock is used
   * like this. So don't complain when you rely on it. 
   */ 
  for (k = 0; k < z; k++) 
    {
      pSave = pSave2;
      for (j = 0; j < y; j++) 
	{
	  p = pSave;
	  for (i = 0; i < x; i++) 
	    {
	      subblock.setPtr (p);
	      add3DBlock (& (go3DBlock<T>)subblock);
	      p += dx;
	    }
	  pSave += dy;
	}
      pSave2 += dz;
    }
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
write (ofstream &f, ofstream &indexFile, 
       ofstream &blockIndexFile, ofstream &parameterFile) {
  goIndex_t i,j;
  goArray<CODE_T> *s;
  CODE_T c;
  goSize_t index;
  switch (coder->getID()) 
    {
    case GO_ID_3DCODERSUBBAND:
      /* Write index file */
      indexFile.write ((char*)indexArray.getPtr(), sizeof(goSize_t) * indexArray.getSize());
//        for (i = 0; i < indexArray.getSize(); i++) 
//  	{
//  	  if (i < (indexArray.getSize() - 1)) 
//  	    {
//  	      indexFile << indexArray[i] << endl;
//  	    } else
//  	      {
//  		indexFile << indexArray[i];
//  	      }
//  	}
      /* Write streams and block index file */
      for (i = 0; i < codedStreams.getSize(); i++) 
	{
	  s = (goArray<CODE_T>*)codedStreams[i];
	  f.write ( (char*)s->getPtr(), 
		    sizeof(CODE_T) * s->getSize());
	  index = s->getSize();
	  blockIndexFile.write ((char*)&index, sizeof(goSize_t));
//  	  if (i < (codedStreams.getSize() - 1)) 
//  	    {
//  	      blockIndexFile << s->getSize() << endl;
//  	    } else {
//  	      blockIndexFile << s->getSize();
//  	    }
	}
      /* Write coder parameters */
      parameterFile.write ((char*)coderParameter.getPtr(), sizeof(unsigned long) * coderParameter.getSize());
//        for (i = 0; i < coderParameter.getSize(); i++)
//  	{
//  	  if (i < (coderParameter.getSize() - 1)) 
//  	    {
//  	      parameterFile << coderParameter[i] << endl;
//  	    } else {
//  	      parameterFile << coderParameter[i];
//  	    }
//  	}
      break;
    default:
      for (i = 0; i < codedStreams.getSize(); i++) 
	{
	  s = (goArray<CODE_T>*)codedStreams[i];
	  index = s->getSize();
	  indexFile.write ((char*)&index, sizeof(goSize_t));
	  // indexFile << s->getSize() << endl;
	  for (j = 0; j < s->getSize(); j++) 
	    {
	      c = (*s)[j];
	      f.write ( (char*)(&c),sizeof(CODE_T) );
	    }
	}
      break;
    }
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
readBlockStage (goSize_t blockIndex1,
		goSize_t blockIndex2,
		goSize_t stage)
{
  ifstream f;
  f.open (volumeFileName.toCharPtr(), ios::in | ios::binary);
  readBlockStage (f, blockIndex1, blockIndex2, stage);
  f.close();
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
readBlockStage (ifstream &f, 
		goSize_t blockIndex1, 
		goSize_t blockIndex2,
		goSize_t stage)
{
  goSize_t fileOffset = 0;
  goSize_t i;
  goSize_t lengthIndex;
  goSize_t length;
  goSize_t stageRelOffset;
  goArray<CODE_T> *s;
  for (i = blockIndex1; i <= blockIndex2; i++) 
    {
      fileOffset = blockOffset[i];
      lengthIndex = i * (stages + 1) + stage;
      stageRelOffset = stageIndex[lengthIndex];
      length = stageLength[lengthIndex];

      /* Annahme: Band (stage) ist noch nicht geladen und ist das nächst
       * höhere Band. --> An codedStream anhängen.
       */
      f.seekg ((fileOffset + stageRelOffset) * sizeof(CODE_T));
      s = (goArray<CODE_T>*)codedStreams[i];
      s->resize (s->getSize() + length);
      f.read ((char*)(s->getPtr() + s->getSize() - length), 
	      sizeof(CODE_T) * length); 
    }
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
setFileNames (const char* volume, 
	      const char* index, 
	      const char* blockindex,
	      const char* parameter) 
{
  volumeFileName = volume;
  indexFileName = index;
  blockIndexFileName = blockindex;
  parameterFileName = parameter;
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
preloadIndices ()
{
  ifstream f, indexFile, blockIndexFile, parameterFile;
  f.open (volumeFileName.toCharPtr(), ios::in | ios::binary);
  indexFile.open (indexFileName.toCharPtr(), ios::in | ios::binary);
  blockIndexFile.open (blockIndexFileName.toCharPtr(), ios::in | ios::binary);
  parameterFile.open (parameterFileName.toCharPtr(), ios::in | ios::binary);
  goError::note ("go3DCompressed::preloadIndices()","");
  switch (coder->getID()) 
    {
    case GO_ID_3DCODERSUBBAND:
      blockOffset.resize(0);
      /* Read index file */
      goSize_t t;
      goSize_t ssize;
      goSize_t i,j;
      t = 0;
      goArray<CODE_T> *s;
      blockOffset += t;
      while (!indexFile.eof()) 
	{
	  indexFile.read ((char*)&t, sizeof(goSize_t));
	  // indexFile >> t;
	  indexArray += t;
	}
      i = 1;
      goSize_t temp_idx;
      goSize_t temp_length;
      goSize_t k;
      stageLength.resize(0);
      while ( (!f.eof()) && (!blockIndexFile.eof()) )
	{
	  s = new goArray<CODE_T>;
	  blockIndexFile.read ((char*)&ssize, sizeof(goSize_t));
	  // blockIndexFile >> ssize;
	  codedStreams.resize(codedStreams.getSize() + 1);

	  temp_idx = codedStreams.getSize() - 1;
	  codedStreams[temp_idx] = (void*)s;
	  s->resize (0);
	  temp_idx     = temp_idx * stages * 8;
	  stageLength += indexArray[temp_idx];
	  /* Längen der stage streams abspeichern */
	  /* Ist das richtig ? Yep.*/
	  for (j = 1; j <= stages; j++) 
	    {
	      temp_length = indexArray[temp_idx + 7] - indexArray[temp_idx];
	      stageLength += temp_length;
	      temp_idx += 8;
	    }

	  t = blockOffset[i-1] + ssize;
	  blockOffset += t;
	  i++;
	}
      stageIndex.resize(0);
      temp_idx = 0;
      for (j = 0; j < (goSize_t)codedStreams.getSize(); j++)
	{
	  stageIndex += 0;
	  temp_length = 0;
	  for (k = 0; k < stages; k++) 
	    {
	      temp_length += stageLength[temp_idx++];
	      stageIndex += temp_length;
	    }
	  temp_idx++;
	}
      /* Read coder parameters */
      coderParameter.resize(0);
      unsigned long l;
      while (!parameterFile.eof())
	{
	  parameterFile.read ((char*)&l, sizeof(unsigned long));
	  // parameterFile >> l;
	  coderParameter += l;
	}
      cout << endl;
      cout << "Size of index array: " << indexArray.getSize() << endl;
      cout << "Size of codedStreams: " << codedStreams.getSize() << endl;
      break;
    default:
      break;
    }
  f.close();
  indexFile.close();
  blockIndexFile.close();
  parameterFile.close();
}
									     

/*
 * Read the whole shit. Use the alternative read function for a choice what to
 * read and at which resolution.
 */
template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
read () 
{

  destroyCodedStreams ();

  ifstream f;
  ifstream indexFile;
  ifstream blockIndexFile;

  goSize_t i,j;
  goArray<CODE_T> *s;
  goSize_t sSize;
  CODE_T c;

  /* Preload index arrays, but do not load actual data.
   * Then, load data resolution and roi dependent!
   */
  switch (coder->getID()) 
    {
    case GO_ID_3DCODERSUBBAND:
      goSize_t ssize;
      preloadIndices();
      f.open (volumeFileName.toCharPtr(), ios::in | ios::binary);
      for (i = 0; i < (goSize_t)codedStreams.getSize(); i++) 
	{
	  s = (goArray<CODE_T>*)codedStreams[i];
	  ssize = blockOffset[i+1] - blockOffset[i];
	  s->resize (ssize);
       	  f.read ( (char*)s->getPtr(), 
		   sizeof(CODE_T) * ssize );
	}
      f.close();
      cout << "Size of index array: " << indexArray.getSize() << endl;
      cout << "Size of codedStreams: " << codedStreams.getSize() << endl;
      break;
    default:
      f.open (volumeFileName.toCharPtr(), ios::in | ios::binary);
      indexFile.open (indexFileName.toCharPtr());
      while (!f.eof()) {
	s = new goArray<CODE_T>;
	codedStreams.resize (codedStreams.getSize() + 1);
	codedStreams[codedStreams.getSize() - 1] = (void*)s;
	indexFile >> sSize;
	s->resize (sSize);
	for (j = 0; j < sSize; j++) {
	  f.read ( (char*)(&c), sizeof(CODE_T) );
	  (*s)[j] = c;
	}
      }
      f.close();
      indexFile.close();
      break;
    }

}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
readAndCompress (ifstream &f, 
		 goSize_t xs, 
		 goSize_t ys, 
		 goSize_t zs) {
  goSize_t xOffset, yOffset;
  goSize_t blockX = 0, blockY = 0, blockZ = 0;
  goSize_t x = 0,y = 0,z = 0;
  cout << "Blocksize: " << blockSize.x << "," << blockSize.y << "," << blockSize.z << endl;
  goSize_t zMax = (goSize_t)(ceil(zs / (float)blockSize.z));
  goSize_t chunkSize = blockSize.z * xs * ys;
  go3DBlock<T> b;

  b.make (xs, ys, blockSize.z);
  // Check this, it's wrong.
  // b.make (xs + blockSize.x, ys + blockSize.y, blockSize.z);
  go3DSubBlock<T> sub;
  sub.setSize (blockSize.x, blockSize.y, blockSize.z);
  sub.setParent (&b);
  for (z = 0; z < zMax; z++) 
    {
      cout << z / (float)zMax * 100 << "% done." << endl;
      memset (b.getPtr(), 0, sizeof (T) * chunkSize);
      /* Read one chunk at a time */
      f.read ((char*)b.getPtr (0,0,0), sizeof(T) * chunkSize);
      /* DANGER .. this does not work with volumes of x and y sizes not a multiple of blocksize! */
      for (yOffset = 0; yOffset < ys; yOffset += blockSize.y)
	{
	  for (xOffset = 0; xOffset < xs; xOffset += blockSize.x) 
	    {
	      sub.setPosition (xOffset,yOffset,0);
	      add3DBlock (&sub);
	    }
	}
    }
  b.destroy();
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
setStages (goSize_t s) {
  stages = s;
  ST.setStages (s);
  if (coder)
    {
      switch (coder->getID()) 
	{
	case GO_ID_3DCODERSUBBAND:
	  ((go3DCoderSubBand<CODE_T>*)coder)->setStages (s);
	  break;
	default:
	  break;
	}
    }
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
add3DBlock (go3DBlock<T> *b) {
  ST.setSTransform (&trans);
  transform (b);
  encode ();
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
set3DCoder (go3DCoder<CODE_T> *c) {
  coder = c;
  switch (coder->getID()) 
    {
    case GO_ID_3DCODERSUBBAND:
      ((go3DCoderSubBand<CODE_T>*)coder)->setIndexArray (&indexArray);
      ((go3DCoderSubBand<CODE_T>*)coder)->setParameterArray (&coderParameter);
      ((go3DCoderSubBand<CODE_T>*)coder)->setRelevantBits (sizeof(T) * 8);
      break;
    default: break;
    }
}

template< class T, class CODE_T >
go3DBlock<T>*
go3DCompressed<T,CODE_T>::
get3DBlock (goIndex_t blockIndex, goSize_t stage) {
#ifdef GO_3DCOMPRESSED_USECACHE
  go3DBlock<T> *cached = cache.get ((goSize_t)blockIndex, blockIndex);
  if (!cached) {
    // cout << "Cache miss, getting block " << blockIndex << "." << endl;
    go3DBlock<T> *b = new go3DBlock<T>;
    // register goInt16 size_shift = stages - stage;
    b->make (blockSize.x >> (stages - stage), 
	     blockSize.y >> (stages - stage),
	     blockSize.z >> (stages - stage));
    /* Decode */
    decode (blockIndex, &trans, stage);
    if (stage == 0)
      {
	trans.setSize (trans.getSizeX() >> stages,
		       trans.getSizeY() >> stages,
		       trans.getSizeZ() >> stages);
	trans.setDiff (trans.getXDiff() << stages,
		       trans.getYDiff() << stages,
		       trans.getZDiff() << stages);
	GO_3D_COPYBLOCK(trans, CODE_T, (*b), T);
	trans.setSize (trans.getSizeX() << stages,
		       trans.getSizeY() << stages,
		       trans.getSizeZ() << stages);
	trans.setDiff (trans.getXDiff() >> stages,
		       trans.getYDiff() >> stages,
		       trans.getZDiff() >> stages);
      } else
	{
	  ST.set3DBlock (b);
	  /* Reconstruct */
	  ST.setSTransform (&trans);
#ifdef GO_3DTAKETIME
	  clock_t t1,t2;
	  t1 = clock();
	  ST.reverse(stage);
	  t2 = clock();
	  transformTime += (t2 - t1);
#else
	  ST.reverse(stage);
#endif
	}
    cache.add (blockIndex, blockIndex, b);
    return b;
  } else {
    // cout << "Cache hit block " << blockIndex << "." << endl;
    return cached;
  }
#else
  // don't use cache
  decode (blockIndex, &trans, stage);
  //  if (stage != stages) {
  returnBlock.setSize (blockSize.x >> (stages - stage), 
		       blockSize.y >> (stages - stage),
		       blockSize.z >> (stages - stage));
//    returnBlock.setDiff (returnBlockDiffX << (stages - stage),
//  		       returnBlockDiffY << (stages - stage),
//  		       returnBlockDiffZ << (stages - stage));
    //  }
  if (stage == 0)
    {
      trans.setSize (trans.getSizeX() >> stages,
		     trans.getSizeY() >> stages,
		     trans.getSizeZ() >> stages);
      trans.setDiff (trans.getXDiff() << stages,
		     trans.getYDiff() << stages,
		     trans.getZDiff() << stages);
      GO_3D_COPYBLOCK(trans, CODE_T, returnBlock, T);
      trans.setSize (trans.getSizeX() << stages,
		     trans.getSizeY() << stages,
		     trans.getSizeZ() << stages);
      trans.setDiff (trans.getXDiff() >> stages,
		     trans.getYDiff() >> stages,
		     trans.getZDiff() >> stages);
    } else
      {
	ST.set3DBlock (&returnBlock);
	/* Reconstruct */
	ST.setSTransform (&trans);
#ifdef GO_3DTAKETIME
	clock_t t1,t2;
	t1 = clock();
	ST.reverse(stage);
	t2 = clock();
	transformTime += (t2 - t1);
#else
	ST.reverse(stage);
#endif
      }
  return &returnBlock;
#endif
}

template< class T, class CODE_T >
go3DBlock<T>*
go3DCompressed<T,CODE_T>::
get3DBlock (goIndex_t blockIndex, goIndex_t x, goIndex_t y, goIndex_t z) {
  go3DBlock<T> *cached = cache.get ((goSize_t)(x + y + z), blockIndex);
  if (!cached) {
    // cout << "Cache miss, getting block " << blockIndex << "." << endl;
    go3DBlock<T> *b = new go3DBlock<T>;
    b->make (blockSize.x, blockSize.y, blockSize.z);
    decode (blockIndex, &trans);
    ST.set3DBlock (b);
    // reconstruct (&trans);
    /* Do the same as reconstruct() */
    ST.setSTransform (&trans);
    // Assume the target block was already set in dwt.
#ifdef GO_3DTAKETIME
    clock_t t1,t2;
    t1 = clock();
    ST.reverse();
    t2 = clock();
    transformTime += (t2 - t1);
#else
    ST.reverse();
#endif
    // dwt.setSTransform (NULL);

    cache.add (x + y + z, blockIndex, b);
    return b;
  } else {
    // cout << "Cache hit block " << blockIndex << "." << endl;
    return cached;
  }
}


template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
destroyCodedStreams () {
  goIndex_t i;
  for (i = 0; i < codedStreams.getSize(); i++) {
    delete ((goArray<CODE_T>*)codedStreams[i]);
  }
  codedStreams.resize(0);
  indexArray.resize(0);
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::
transform (go3DBlock<T> *b) {
#ifdef GO_3DTAKETIME
  clock_t t1,t2;
  ST.set3DBlock (b);
  t1 = clock();
  ST.execute();
  t2 = clock();
  transformTime += (t2 - t1);
#else
  ST.set3DBlock (b);
  ST.execute();
#endif
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::encode () {
#ifdef GO_3DTAKETIME
  clock_t t1,t2;
#endif
  if (!coder) {
    goError::print ("go3DCompressed::encode()","No coder set.");
    return;
  }

  goArray<CODE_T> *codedStream = new goArray<CODE_T>;
  coder->set3DBlock (ST.getSTransform());
  codedStreams.resize (codedStreams.getSize() + 1);
  codedStreams[codedStreams.getSize() - 1] = (void*)codedStream;

  switch (coder->getID()) {
  case GO_ID_3DCODERSUBBAND:
    // cout << "Subband coder detected." << endl;
    ((go3DCoderSubBand<CODE_T>*)coder)->setCodedStream (codedStream);
#ifdef GO_3DTAKETIME
    t1 = clock();
    ((go3DCoderSubBand<CODE_T>*)coder)->encode();
    t2 = clock();
    coderTime += (t2 - t1);
#else
    ((go3DCoderSubBand<CODE_T>*)coder)->encode();
#endif
    break;
  case GO_ID_3DCODERFELICS:
    // cout << "FELICS coder detected." << endl;
    coder->setCodedStream (codedStream);
    ((go3DCoderFELICS<CODE_T>*)coder)->encode();
    break;
  case GO_ID_3DCODER:
    // cout << "Empty coder detected." << endl;
    coder->setCodedStream (codedStream);
    coder->encode();
    break;
  default:
    goError::print ("go3DCompressed::encode()","Unknown coder.");
    break;
  }
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::decode (goIndex_t blockIndex, 
				  go3DBlock<CODE_T> *b,
				  goSize_t stage) {
#ifdef GO_3DTAKETIME
  clock_t t1,t2;
#endif
  coder->set3DBlock (b);

  switch (coder->getID()) {
  case GO_ID_3DCODERSUBBAND:
    ((go3DCoderSubBand<CODE_T>*)coder)->setCodedStream((goArray<CODE_T>*)codedStreams[blockIndex]);
#ifdef GO_3DTAKETIME
    t1 = clock();
    ((go3DCoderSubBand<CODE_T>*)coder)->decode(stage,blockIndex * 8 * stages);
    t2 = clock();
    coderTime += (t2 - t1);
#else
    ((go3DCoderSubBand<CODE_T>*)coder)->decode(stage,blockIndex * 8 * stages);
#endif
    break;
  case GO_ID_3DCODERFELICS:
    coder->setCodedStream ((goArray<CODE_T>*)codedStreams[blockIndex]);
    ((go3DCoderFELICS<CODE_T>*)coder)->decode();
    break;
  case GO_ID_3DCODER:
    coder->setCodedStream ((goArray<CODE_T>*)codedStreams[blockIndex]);
    coder->decode();
    break;
  default:
    goError::print ("go3DCompressed::encode()","Unknown coder.");
    break;
  }
}

template< class T, class CODE_T >
void
go3DCompressed<T,CODE_T>::reconstruct (go3DBlock<CODE_T> *b) {
  ST.setSTransform (b);
  // Assume the target block was already set in dwt.
  ST.reverse();
  // dwt.setSTransform (NULL);
}

template< class T, class CODE_T >
goSize_t
go3DCompressed<T,CODE_T>::getSize() {
  goIndex_t i,s = 0;
  for (i = 0; i < codedStreams.getSize(); i++) {
    s += ((goArray<CODE_T>*)codedStreams[i])->getSize();
  }
  return (s * sizeof(CODE_T));
}


template class go3DCompressed< goInt8, goInt32 >;
template class go3DCompressed< goUInt8, goInt32 >;
template class go3DCompressed< goInt16, goInt32 >;

 
