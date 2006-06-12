#ifndef GOQUANTIZER_HPP
#define GOQUANTIZER_HPP
#include <goquantizer.h>
#include <gouniformquantizer.h>
#ifndef GODEFS_H
# include <godefs.h>
#endif

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
#endif
