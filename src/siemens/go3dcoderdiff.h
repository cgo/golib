#ifndef GO3DCODERDIFF_H
#define GO3DCODERDIFF_H

#include <gotypes.h>
#include <go3dcoder.h>

#define 

template < class T >
class
go3DCoderDiff : public go3DCoder {
 public:
  go3DCoderDiff ();
  virtual ~go3DCoderDiff ();

  void init (void*);

  void encode ();
  void decode ();
 protected:
};

#endif
