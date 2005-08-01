#include <golist.h>
#include <golist.hpp>
#include <gostring.h>
#include <gopoint.h>
#include <go4vector.h>
#include <gocurve.h>
#include <goindexpair.h>

template class goListElement<goInt8>;
template class goListElement<goUInt8>;
template class goListElement<goInt16>;
template class goListElement<goUInt16>;
template class goListElement<goInt32>;
template class goListElement<goUInt32>;
template class goListElement<goFloat>;
template class goListElement<goDouble>;
template class goListElement<goString>;
template class goListElement<void*>;
template class goListElement<goPointf>;
template class goListElement<goPointd>;
template class goListElement<goCurve>;
template class goListElement<goCurve*>;
template class goListElement<go4Vectorf>;
template class goListElement<goIndexPair>;

template class goList<goInt8>;
template class goList<goUInt8>;
template class goList<goInt16>;
template class goList<goUInt16>;
template class goList<goInt32>;
template class goList<goUInt32>;
template class goList<goFloat>;
template class goList<goDouble>;
template class goList<goString>;
template class goList<void*>;
template class goList<goPointf>;
template class goList<goPointd>;
template class goList<goCurve>;
template class goList<goCurve*>;
template class goList<go4Vectorf>;
template class goList<goIndexPair>;
