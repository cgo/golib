#ifndef GO3DCODERHUFFMAN_H
#define GO3DCODERHUFFMAN_H

#include <go3dcoder.h>
#include <gocodebook.h>

template< class T >
class
go3DCoderHuffman : public go3DCoder {
 public:
  go3DCoderHuffman ();
  virtual ~go3DCoderHuffman ();

  /*!
   * ptr is a pointer to a ... of which the codebook
   * will be made.
   */
  void		init (void* ptr);

  void		encode ();
  void		decode ();

 protected:
  goCodeBook<T>	codeBook;
};

#endif
