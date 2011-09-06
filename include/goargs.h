/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOARGS_H
#define GOARGS_H

#include <stdio.h>
#include <gostring.h>

bool goReadASCIIKeywordArg (FILE* file, goString& keyword, goString& arg, const char* checkKeyword);

#endif
