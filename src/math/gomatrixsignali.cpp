/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomatrix.h>
// #include <gomatrix.hpp>
#include <gocomplex.h>
#include <gomath.h>		// MAX()
#include <iostream>

template <>
goComplexf goMatrix<goComplexf>::norm () const
{
    goLog::error("goMatrix::norm not implemented for complex types.");
    return goComplexf(0.0f,0.0f);
}

template <>
goComplexd goMatrix<goComplexd>::norm () const
{
    goLog::error("goMatrix::norm not implemented for complex types.");
    return goComplexd(0.0f,0.0f);
}

/* Instantiation */
template class goMatrix<goDouble>;
template class goMatrix<goFloat>;
template class goMatrix<goInt32>;
template class goMatrix<goComplexf>;
