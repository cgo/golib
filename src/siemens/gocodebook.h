#ifndef GOCODEBOOK_H
#define GOCODEBOOK_H

#include <goarray.h>

template< class T >
class
goCodeBook {
 public:
  goCodeBook ();
  virtual ~goCodeBook ();

  void add (T word, goIndex_t position);
  T& operator[] (goIndex_t idx);
  goArray<T> *getArray () { return &array; }

 protected:
  goArray<T> array;
};

#endif
