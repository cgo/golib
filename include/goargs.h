#ifndef GOARGS_H
#define GOARGS_H

#include <stdio.h>
#include <gostring.h>

bool goReadASCIIKeywordArg (FILE* file, goString& keyword, goString& arg, const char* checkKeyword);

#endif
