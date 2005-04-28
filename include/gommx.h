#ifndef GOMMX_H
#define GOMMX_H

#include <goconfig.h>

extern "C" int goASMCheckMMX();		// src/mmx/gommxcheck.s
extern "C" int goASMCheck3DNOW();	// src/mmx/gommxcheck.s
extern "C" int goASMCheck3DNOWDSP();	// src/mmx/gommxcheck.s
extern "C" int goASMCheck3DNOWEXT();	// src/mmx/gommxcheck.s

#ifdef GO_HAVE_MMX
extern "C" void goASMMMXBegin();
extern "C" void goASMMMXEnd();
#endif
#ifdef GO_HAVE_3DNOW
extern "C" void goASM3DNBegin();
extern "C" void goASM3DNEnd();
#endif

/*!
 * Check for MMX instruction set
 */ 
bool goCheckMMX()
{
  int i = goASMCheckMMX();
  if (i == 1)
    {
      return true;
    }
  return false;
}

/*!
 * Check for AMD's 3DNOW! instruction set
 */
bool goCheck3DNOW()
{
  int i = goASMCheck3DNOW();
  if (i == 1)
    {
      return true;
    }
  return false;
}

/*!
 * Check for AMD's DSP extensions to 3DNOW! in their Athlon processors
 */
bool goCheck3DNOWDSP()
{
  int i = goASMCheck3DNOWDSP();
  if (i == 1)
    {
      return true;
    }
  return false;
}

/*!
 * Check for AMD's extensions to MMX in their Athlon processors
 */
bool goCheck3DNOWEXT()
{
  int i = goASMCheck3DNOWEXT();
  if (i == 1)
    {
      return true;
    }
  return false;
}
#endif
