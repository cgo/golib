/* --*- C++ -*-- */
#include <goarray.h>
#include <goarray.hpp>
#include <gocomplex.h>
#include <goconfigfile.h>
#include <golist.h>
#include <gopoint.h>
#include <gohashtable.h>

// instantiation
template class goArray< goInt8 >;
template class goArray< goUInt8 >;
template class goArray< goInt16 >;
template class goArray< goUInt16 >;
template class goArray< goInt32 >;
template class goArray< goUInt32 >;
template class goArray< goInt64 >;
template class goArray< goUInt64 >;
template class goArray< goFloat >;
template class goArray< goDouble >;
template class goArray< void* >;

// added for goNVector
//template class goArray<goUInt32>;
//template class goArray<goFloat>;
template class goArray<goComplex<goDouble> >;

// more exotic ones
template class goArray<goPointf>;
template class goArray<goPointd>;
template class goArray<goString* >;
template class goArray<goArray<int>* >;
template class goArray<goConfigFileEntry* >;
template class goArray<goConfigFileSection* >;
template class goArray<goConfigFileChapter* >;
//template class goArray<goList<goHashEntry<goUInt64, void*> > >;
//template class goArray<goList<goHashEntry<goUInt32, void*> > >;
template class goArray< void* (*)(void*) >;

