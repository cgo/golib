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
