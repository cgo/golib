#ifndef GO3DCODERFELICS_H
#define GO3DCODERFELICS_H

#include <go3dcoder.h>

/*!
 *
 */
template< class T >
class
go3DCoderFELICS : public go3DCoder<T> {
 public:
  go3DCoderFELICS ();
  virtual ~go3DCoderFELICS ();

  void		init (void* ptr);

  void		encode ();
  void		decode ();

  /*!
   * This must be set before doing anything with the coder!
   * Set to the maximum of used bits per word in the unencoded data.
   */
  void		setRelevantBits (goSize_t b);
 protected:

 private:
  goSize_t relevantBits;
};

#endif
