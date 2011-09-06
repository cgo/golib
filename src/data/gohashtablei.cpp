/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gohashtable.h>
#include <gohashtable.hpp>
#include <goindexpair.h>
#include <gostring.h>

//= For goSparseMatrix multiplication
template class goHashTable<goIndexPair, goDouble>;
template class goHashTable<goIndexPair, goUInt8>;
template class goHashTable<int, goString>;
template class goHashTable<goUInt64, void*>;
