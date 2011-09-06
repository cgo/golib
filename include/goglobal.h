/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGLOBAL_H
#define GOGLOBAL_H

#include <goconfig.h>
#ifndef GOHASHTABLE_H
# include <gohashtable.h>
#endif
#ifndef GOSTRING_G
# include <gostring.h>
#endif

namespace goGlobal
{
    extern bool ILInitialized;       // Indicates if DevIL is
                                     // initialized (if it is 
                                     // used at all).
#ifdef GO_USE_CLASSNAMES
    class goClassNameTable : public goHashTable<int,goString>
    {
        public:
            goClassNameTable();
            virtual ~goClassNameTable();
    };
#else
    class goClassNameTable 
    {
        public:
            goClassNameTable();
            ~goClassNameTable();
            goString& operator[] (int);
    };
#endif
    extern goClassNameTable classNames;
}

#endif
