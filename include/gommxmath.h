#ifndef GOMMXMATH_H
#define GOMMXMATH_H

#include <config.h>

#ifdef GO_HAVE_3DNOW
extern "C" void goASM3DNMult2f (float f1, float f2, float f3, float f4, float* result);
#endif

#endif
