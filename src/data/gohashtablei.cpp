#include <gohashtable.h>
#include <gohashtable.hpp>
#include <goindexpair.h>

//= For goSparseMatrix multiplication
template class goHashTable<goIndexPair, goDouble>;
template class goHashTable<goIndexPair, goUInt8>;
