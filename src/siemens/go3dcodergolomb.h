#ifndef GO3DCODERGOLOMB_H
#define GO3DCODERGOLOMB_H

#include <go3dcoder.h>
#include <gotypes.h>
#include <iostream.h>

/*!
 * \todo Change INTSIZE to a type dependent setting. 32 bits holds only for int.
 * \todo encode(), decode(): delta = 0 works better than taking delta = *(line-dx) ??
 * @author Christian Gosch
 */
template <class T>
class
go3DCoderGolomb : public go3DCoder<T> {
 public:
  go3DCoderGolomb ();
  virtual ~go3DCoderGolomb ();

  void	init (void* ptr);

  void	encode ();
  void	decode ();

  inline unsigned long   getParameter() { return parameter; }
  inline void		 setParameter(unsigned long k) { parameter = k; }
  /*! 
   * This must be set before doing anything with the coder!
   * Set to the maximum of used bits per word in the unencoded data.
   * @see <code>go3DCoderFELICS</code>
   */
  void setRelevantBits (goSize_t b);

 private:
  /*!
   * @return Number of consecutive zero bits.
   * There is a macro, <code>GET_ZERO_BITS</code>, in the source code file 
   * to speed up this operation. 
   */
  unsigned int	getZeroBits ();
  /*!
   * @return Binary part of length <code>parameter</code> bits.
   * There is a macro, <code>GET_BINARY_PARTS</code>, in the source code file 
   * to speed up this operation. 
   */
  goUInt32	getBinaryPart ();
  /*!
   * Calculates the best parameter for the <strong>whole block</strong>.
   * Used by encode() to determine the parameter for the Golomb-Rice code.
   */
  void guessParam (unsigned int max_cumul);

  goSize_t relevantBits; 
  T maxval;
  unsigned long parameter;


  
  int	bits;			// bit stream pointer
  int	bitsMin;		// number of bits per word - 8
  goUInt32  bitBuff;		// bit buffer
  int  bytesPerWord;		// 
  int  bitsPerWord;		// 
  int  bytesPerT;		// bytes per data type T
  goUInt8 *Tbuff;		// 8-bit-word buffer

  unsigned int	zeroTable[256]; // Table mapping 8 bit values to the corresponding
				// number of leading zeroes (filled by the constructor)
  
  goUInt8* streamPtr;		// Pointer to the stream data cast to 8 bit words
  
};

#endif



