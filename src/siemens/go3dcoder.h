#ifndef GO3DCODER_H
#define GO3DCODER_H

#include <gotypes.h>
#include <go3dblock.h>
#include <goarray.h>

/*!
 * Coder class to encode/decode go3DBlock data.
 * Create subclasses to implement different coders.
 * This class implements the empty coder (writes raw data into output stream).
 */
template< class T >
class
go3DCoder {
 public:
  go3DCoder ();
  virtual ~go3DCoder ();

  void		setCodedStream (goArray<T> *s, goSize_t idx = 0);
  inline void	setStreamIndex (goSize_t idx) 
    {
      streamIndex = idx;
    }
  void		set3DBlock (go3DBlock<T> *b);
  goArray<T>	*getCodedStream ();
  go3DBlock<T>	*get3DBlock ();

  void	init (void* ptr);

  void	encode ();
  void	decode ();

  int		getID ();
  
  /// Dummy
  unsigned long getParameter () { return 0; }
  /// Dummy
  void setParameter (unsigned long) { }

 protected:
  goArray<T>	*codedStream;
  goSize_t	streamIndex;
  go3DBlock<T>	*block;
  int		ID;
};

#endif
