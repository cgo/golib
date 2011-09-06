/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include "goerror.h"
class goString;

void
goError::print (const char *caller, const char *s) {
    std::cout << caller << ": golib error: " << s << std::endl;
}

void
goError::print (const char *caller, const goString& s) {
    std::cout << caller << ": golib error: " << s << std::endl;
}

void
goError::note (const char* caller, const char* s) {
    std::cout << caller << ": " << s << std::endl;
}

void
goError::note (const char* caller, const goString& s) {
    std::cout << caller << ": " << s << std::endl;
}
