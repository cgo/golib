#include <gohashtable.h>
#include <gohashtable.hpp>
#include <goindexpair.h>
#include <gostring.h>

//= For goSparseMatrix multiplication
template class goHashTable<goIndexPair, goDouble>;
template class goHashTable<goIndexPair, goUInt8>;
template class goHashTable<int, goString>;
template class goHashTable<goUInt64, void*>;
