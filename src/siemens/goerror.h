#ifndef __GOERROR_H__
#define __GOERROR_H__

#include <iostream.h>

class
goError {
 public:
  static void print (const char*, const char*);
  static void note  (const char*, const char*);
};

#endif
