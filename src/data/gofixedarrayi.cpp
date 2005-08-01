#include <gofixedarray.h>
#include <gofixedarray.hpp>
#include <gopoint.h>
#include <gocomplex.h>
#include <govector.h>

template class goFixedArray<goFloat>;
template class goFixedArray<goDouble>;
template class goFixedArray<goPointf>;
template class goFixedArray<goVectorf>;
template class goFixedArray<goComplexf>;
template class goFixedArray< goVector<goComplexf> >;
