#include <golist.h>
#include <golist.hpp>
// #include <gostring.h>

template class goListElement<goInt8>;
template class goListElement<goUInt8>;
template class goListElement<goInt16>;
template class goListElement<goUInt16>;
template class goListElement<goInt32>;
template class goListElement<goUInt32>;
template class goListElement<goFloat>;
template class goListElement<goDouble>;
template class goListElement<void*>;

template class goList<goInt8>;
template class goList<goUInt8>;
template class goList<goInt16>;
template class goList<goUInt16>;
template class goList<goInt32>;
template class goList<goUInt32>;
template class goList<goFloat>;
template class goList<goDouble>;
template class goList<void*>;
