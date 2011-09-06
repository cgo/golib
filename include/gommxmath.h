/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOMMXMATH_H
#define GOMMXMATH_H

#include <goconfig.h>

#ifdef GO_HAVE_3DNOW
extern "C" void goASM3DNMult2f (float f1, float f2, float f3, float f4, float* result);
#endif

#endif
