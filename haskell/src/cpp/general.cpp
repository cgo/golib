#include "general.h"

int golib_check_null_ptr (void* p)
{
  if (0 == p)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}
