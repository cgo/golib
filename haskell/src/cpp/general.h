/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef _GENERAL_H_
#define _GENERAL_H_

#ifdef __cplusplus 
extern "C"
{
#endif
#include <stdlib.h>

  int golib_check_null_ptr (void* p);
  static inline int fromBool (bool b)
  {
    switch (b)
      {
      case true: return 1; break;
      case false: return 0; break;
      }
  }

 
#ifdef __cplusplus 
}
#endif

#endif /* _GENERAL_H_ */
