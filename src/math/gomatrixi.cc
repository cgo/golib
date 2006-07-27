#include <gomatrix.h>
#include <gomatrix.hpp>
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
// template class goMatrix<goInt32>;
template class goMatrix<goComplexf>;
