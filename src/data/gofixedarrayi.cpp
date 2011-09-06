/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
