/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
