#ifndef GO3DCODERSUBBAND_H
#define GO3DCODERSUBBAND_H

#include <go3dcoder.h>
#include <go3dcoderfelics.h>
#include <go3dcodergolomb.h>

/*!
 * Coder for go3DBlock data that has been decomposed using
 * a dyadic transform such as go3DDWT::st().
 * The subbands are coded independently.
 * @author Christian Gosch
 * @see go3DCoder, go3DCoderFELICS, go3DCoderGolomb
 */
template< class T >
class
go3DCoderSubBand : public go3DCoder<T> {
 public:
  go3DCoderSubBand ();
  virtual ~go3DCoderSubBand ();

  void init (void* ptr);
  
  /*!
   * Encodes block set by <CODE>set3DBlock()</CODE> and saves the 
   * resulting bitstream in an array. Retrieve a pointer to that
   * <CODE>goArray<T></CODE> with the method <CODE>getCodedStream()</CODE>.
   * An index to the subbands is maintained. Get the pointer to the
   * <CODE>goArray<goSize_t></CODE> with <CODE>getBandIndexArray()</CODE>.
   * \todo Optimise encode() or replace the whole coder.
   * arrayIndex is not used in encode(), only in decode().
   */ 
  void encode (goSize_t arrayIndex = 0);
  /*!
   * arrayIndex: index to the indexArray where to find the first index to the
   * detail bands.
   */
  void decode (goSize_t decodeStages = 1,
	       goSize_t arrayIndex = 0);

  void setStages (goSize_t s) { stages = s; }
  void setCodedStream (goArray<T> *s, goSize_t idx = 0)
    {
      codedStream = s;
      streamIndex = idx;
      loPassCoder.setCodedStream (s,idx);
      hiPassCoder.setCodedStream (s,idx);
    }
  goArray<goSize_t>*	getIndexArray() { return bandIndex; }
  void			setIndexArray(goArray<goSize_t>* a) { bandIndex = a; }
  goArray<unsigned long>*	getParameterArray() { return coderParameter; }
  void				setParameterArray(goArray<unsigned long>* a) { coderParameter = a; }

  /*!
   * @see <code>go3DCoderGolomb</code>
   */
  void setRelevantBits (goSize_t b)
    {
      hiPassCoder.setRelevantBits (b);
      // loPassCoder.setRelevantBits (b);
    }
  
 protected:

 private:
  go3DCoderGolomb<T> hiPassCoder;
  // go3DCoder<T> hiPassCoder;
  // go3DCoderFELICS<T> loPassCoder;
  go3DCoder<T> loPassCoder;
  
  goArray<goSize_t>* bandIndex;
  goArray<unsigned long>* coderParameter;

  goSize_t stages;
};

#endif





