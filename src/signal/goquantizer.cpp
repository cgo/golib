/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goquantizer.h>
#include <gouniformquantizer.h>

template <class _input_type, class _output_type>
goQuantizer<_input_type, _output_type>::goQuantizer ()
    : goObjectBase ()
{
    this->setClassID(GO_QUANTIZER);
}

template <class _input_type, class _output_type>
goQuantizer<_input_type, _output_type>::~goQuantizer ()
{
}

template <class _input_type, class _output_type>
_output_type
goQuantizer<_input_type, _output_type>::quantize (_input_type input) const
{
    return (_output_type)input;
}

template class goQuantizer <goFloat, goInt8>;
