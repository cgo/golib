#include <go3dcodergolomb.h>
#include <godefs.h>
#include <bitio_m.h>
#include <bitio_m_stdio.h>
#include <gotypes.h>
#include <go3dblock.h>
#include <goarray.h>
#include <gohashtable.h>

#include <iostream.h>

#include <stdlib.h>
#include <string.h>

#define INTSIZE 32

template< class T >
go3DCoderGolomb<T>::
go3DCoderGolomb () : go3DCoder<T>() {
  parameter = 1;
  ID = GO_ID_3DCODERGOLOMB;

  bytesPerWord  = sizeof(bitBuff);
  bitsPerWord   = bytesPerWord << 3;
  bytesPerT	= sizeof(T);
  Tbuff		= new goUInt8[bytesPerT];
  bits		= bitsPerWord;
  bitsMin	= bits - 8;
  bitBuff	= 0;

  // Fill zero table
  int i,j,n;
  int N = 0;
  j = 1;
  n = 8;
  for (i = 0; i < 256; i++)
    {
      if (i >= j) {
	j <<= 1;
	n--;
      }
      zeroTable[i] = n;
    }
}

template< class T >
go3DCoderGolomb<T>::
~go3DCoderGolomb () {
  delete[] Tbuff;
}

template< class T >
void
go3DCoderGolomb<T>::
init (void*) {
  parameter = 1;
}

template< class T >
void
go3DCoderGolomb<T>::
setRelevantBits (goSize_t b) {
  relevantBits = b;
  maxval = (1 << (relevantBits + 3)) - 1;
  // cout << "maxval = " << maxval << endl;
}

template <class T>
void
go3DCoderGolomb<T>::
guessParam (unsigned int max_cumul)
{
  unsigned long cumul_param[INTSIZE];  
  memset (cumul_param, 0, sizeof (cumul_param));
  goArray<int> paramStatsArray;
  paramStatsArray.resize(INTSIZE);
  T *line;
  T* yline;
  T* zline;
  goPtrdiff_t dx = block->getXDiff();
  goPtrdiff_t dy = block->getYDiff();
  goPtrdiff_t dz = block->getZDiff();
  line = block->getPtr (0,0,0);
  yline = line;
  zline = line;
  goSize_t x,y,z;
  T P;
  memset (paramStatsArray.getPtr(), 0, sizeof (int) * paramStatsArray.getSize());
  for (z = 0; z < block->getSizeZ(); z++) {
    yline = zline;
    for (y = 0; y < block->getSizeY(); y++) {
      line = yline;
      for (x = 0; x < block->getSizeX(); x++)
	{
	  P = *(line);
	  // Rice mapping
  	  if (P < 0) {
  	    P *= -1;
	    P = (P << 1) - 1;
  	  } else {
	    P = P << 1;
  	  }
	  /* Find out the best current parameter */
	  unsigned long parm = 0, min, i;
	  long delta;
	  min = cumul_param[0];
	  for (i = 1; i < max_cumul; i++)
	    if (cumul_param[i] < min)
	      {
		min = cumul_param[i];
		parm = i;
	      }
  	  paramStatsArray[parm]++;
	  for (i = 0; i < max_cumul; i++)
	    {
	      cumul_param[i] += (P >> i) + 1 + i;
	    }
	  line += dx;
	}
      yline += dy;
    }
    zline += dz;
  }
  // Guess parameter by taking the one which was used most often.
  // ZERO IS NOT USED AS PARAMETER SINCE IT FUCKS UP THE CODER!
  // Change the coder if you feel like it or if the coder performance is very much compromised
  // by this.
  int maxCount = paramStatsArray[1];
  parameter = 1;
  goIndex_t i;
//    for (i = 0; i < paramStatsArray.getSize(); i++)
//      {
//        cout << "Parameter " << i << ": " << paramStatsArray[i] / (float)(block->getSizeX() * 
//  									block->getSizeY() * 
//  									block->getSizeZ());
//        cout << endl;
//      }
//    char c;
//    cin >> c;
  for (i = 2; i < paramStatsArray.getSize(); i++)
    {
      if (paramStatsArray[i] > maxCount)
	{
	  parameter = i;
	}
    }
}

#define GO_GOLOMBENCODE_START() {		\
  int byteCount = 0;			\
  bits = bytesPerWord << 3;			\
  bitBuff = 0;					

#define GO_GOLOMBENCODE_END() }

#define BINARY_OUT(b) {					\
  int __i;						\
  for (__i = sizeof(b) * 8 - 1; __i >= 0; __i--)	\
    {							\
      cout << (int)((b >> __i) & 1);				\
    } 							\
}

#define PUT_ZEROS(num) {			\
  bits -= num;					\
  while (bits <= bitsMin)			\
    {						\
      if (byteCount >= bytesPerT)		\
	{					\
          T __temp = 0;								\
          int __tempI;								\
	  for (__tempI = 0; __tempI < bytesPerT; __tempI++)			\
	    {									\
	      __temp |= ((T)Tbuff[__tempI]) << ((__tempI) * 8);	\
	    }									\
	  (*codedStream) += __temp;		\
	  byteCount = 0;			\
	}					\
      Tbuff[byteCount++] = bitBuff >> bitsMin;	\
      /* cout << "put byte ";  */\
      /*BINARY_OUT(Tbuff[byteCount - 1]); */\
      /*cout << endl;*/\
      bitBuff <<= 8;				\
      bits += 8;				\
    }						\
}

#define PUT_BITS(w,num) {				\
  bits -= num;						\
  bitBuff |= w << bits;					\
  while (bits <= bitsMin)				\
    {							\
      if (byteCount >= bytesPerT)			\
	{						\
          T __temp = 0;								\
          int __tempI;								\
	  for (__tempI = 0; __tempI < bytesPerT; __tempI++)			\
	    {									\
	      __temp |= ((T)Tbuff[__tempI]) << ((__tempI) << 3);	\
	    }									\
	  (*codedStream) += __temp;		\
	  byteCount = 0;				\
	}						\
      Tbuff[byteCount++] = bitBuff >> bitsMin;		\
      /*cout << "put byte ";*/\
      /*BINARY_OUT(Tbuff[byteCount - 1]); */\
      /*cout << endl;*/\
      bits += 8;					\
      bitBuff <<= 8;					\
    }							\
}

#define FLUSH_BITS() {							\
  while (bits <= bitsPerWord) 								 \
    {											 \
      if (byteCount >= bytesPerT)							 \
	{										 \
	  T __temp = 0;									 \
	  int __tempI;									 \
	  for (__tempI = 0; __tempI < bytesPerT; __tempI++)				 \
	    {    									 \
	      __temp |= ((T)Tbuff[__tempI]) << ((__tempI) << 3);			 \
	    }										 \
	  (*codedStream) += __temp;							 \
	  byteCount = 0;								 \
	}										 \
      Tbuff[byteCount++] = bitBuff >> bitsMin;						 \
      bits += 8;									 \
      bitBuff >>= 8;									 \
    }											 \
  if (byteCount > 0 )								 \
    {											 \
      T __temp = 0;									 \
      int __tempI;									 \
      for (__tempI = 0; __tempI < byteCount; __tempI++)					 \
	{    										 \
	  __temp |= ((T)Tbuff[__tempI]) << ((__tempI) << 3);				 \
	}										 \
      (*codedStream) += __temp;								 \
      byteCount = 0;									 \
    }											 \
}

template< class T >
void
go3DCoderGolomb<T>::
encode ()
{
  goSize_t x, y, z;
  long P;
  unsigned int max_cumul;
  CEILLOG_2 (maxval, max_cumul);
  // guessParam (max_cumul);
  guessParam (relevantBits + 3);
    T *line;
  T* yline;
  T* zline;
  goPtrdiff_t dx = block->getXDiff();
  goPtrdiff_t dy = block->getYDiff();
  goPtrdiff_t dz = block->getZDiff();
  line = block->getPtr (0,0,0);
  yline = line;
  zline = line;
  GO_GOLOMBENCODE_START()
  for (z = 0; z < block->getSizeZ(); z++) {
    yline = zline;
    for (y = 0; y < block->getSizeY(); y++) {
      line = yline;
      for (x = 0; x < block->getSizeX(); x++)
	{
	  P = *(line);
	  // Rice mapping
  	  if (P < 0) {
  	    P *= -1;
	    P = (P << 1) - 1;
  	  } else {
	    P = P << 1;
  	  }
	  
	  /* Now code P as a Golomb-Rice code */
	  unsigned long parm = parameter;
	  PUT_ZEROS((P >> parm));
	  // binary with the trailing 1 from unary
	  goUInt32 v = ((1 << parm) + (P & ((1 << parm) - 1)));
	  PUT_BITS( v, (parm + 1));

	  line += dx;
	}
      yline += dy;
    }
    zline += dz;
  }
  FLUSH_BITS();
  GO_GOLOMBENCODE_END();
  
}

#define GO_GOLOMBDECODE_START() {		\
  int __binaryShift = bitsPerWord - parameter;  \
  register unsigned int __nobits;		\
  register goUInt8 __b = 0;			

#define GO_GOLOMBDECODE_END() }

#define GET_BYTE() {\
  GET_BITS(8)\
}

#define GET_BITS(num) {									\
  bits += num;\
  bitBuff <<= num;									\
  while (bits >= 0)									\
    {											\
      bitBuff |= ((goUInt32)(*(streamPtr++))) << bits;						\
      bits -= 8;									\
/*      cout << "next bits, bitBuff = "; BINARY_OUT(bitBuff); cout << endl;*/\
/*      cout << "bits = " << bits << endl;*/\
    }											\
}

#define GET_BINARY_PART(__bin) {					\
  /* __b = (bitBuff >> __binaryShift) & 0xff; */       		\
  __bin = (bitBuff >> __binaryShift);			\
  GET_BITS(parameter);						\
}

template<class T>
goUInt32
go3DCoderGolomb<T>::
getBinaryPart()
{
  goUInt32 retval;
  retval = (bitBuff >> ((bytesPerWord << 3) - parameter));
  GET_BITS(parameter);
  return retval;
}

#define GET_ZERO_BITS(__z) {				\
  __z = 0;						\
  while (true)						\
    {							\
      __b = (goUInt8)((bitBuff >> bitsMin) & 0xff);	\
      __nobits = zeroTable[__b];				\
      __z += __nobits;					\
      if (__b == 0)					\
	{						\
	  GET_BYTE();					\
	} else						\
	  {						\
	    GET_BITS(__nobits + 1);			\
	    break;					\
	  }						\
    }							\
}

template<class T>
unsigned int
go3DCoderGolomb<T>::
getZeroBits ()
{
  goUInt8 b = 0;
  unsigned int N = 0;
  unsigned int nobits;
  while (true)
    {
      b = (goUInt8)((bitBuff >> bitsMin) & 0xff);
      nobits = zeroTable[b];
      N += nobits;
      // bitBuff <<= nobits;
      // bits += nobits;
      if (b == 0)
	{
	  // refill bit buffer
	  GET_BYTE();
	} else
	  {
	    GET_BITS(nobits + 1);
	    break;
	  }
    }
  return N;
}

template< class T >
void
go3DCoderGolomb<T>::
decode ()
{
  streamPtr = (goUInt8*) (codedStream->getPtr() + streamIndex);
  bits = 0;
  bitBuff = 0;
  GET_BITS(bitsMin);
  goSize_t x, y, z;
  register long P;
  
  register T* line;
  T* yline;
  T* zline;
  register goPtrdiff_t dx = block->getXDiff();
  goPtrdiff_t dy = block->getYDiff();
  goPtrdiff_t dz = block->getZDiff();
  line = block->getPtr (0,0,0);
  yline = line;
  zline = line;
  register unsigned long unary, binary;	    
  GO_GOLOMBDECODE_START()
  for (z = 0; z < block->getSizeZ(); z++) {
    // cout << "z = " << z << endl;
    yline = zline;
    for (y = 0; y < block->getSizeY(); y++) {
      line = yline;
      for (x = 0; x < block->getSizeX(); x++)
	{
	  GET_ZERO_BITS(unary);
	  // unary = getZeroBits();
	  //cout << "Decoded unary: " << unary << endl;
	  // binary = getBinaryPart();
	  GET_BINARY_PART(binary);
	  //cout << "Decoded binary: " << binary << endl;
	  P = (unary << parameter) + binary; 

	  // Do inverse Rice mapping
	  if ( (P & 1) == 1 )
	    {
	      P++;
	      P = -(P >> 1);
	    } else
	      {
		P = P >> 1;
	      }
	  //cout << "After inverse Rice mapping P = " << P << endl;
	  //char c;
	  //cin >> c;
	    
	  *line = (T)P;
	  line += dx;
	}
      yline += dy;
    }
    zline += dz;
  }
  GO_GOLOMBDECODE_END();
}



template class go3DCoderGolomb< goInt32 >;
template class go3DCoderGolomb< goInt16 >;






