#ifndef __GOERROR_H__
#define __GOERROR_H__

#include <iostream>
#include <gostring.h>

namespace goError {
  void print (const char*, const char*);
  void print (const char*, const goString& s);
  void note  (const char*, const char*);
  void note  (const char*, const goString& s);
}

#endif
