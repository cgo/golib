#include <gouniformquantizer.h>

template <class _input_type, class _output_type>
goUniformQuantizer<_input_type, _output_type>::goUniformQuantizer ()
    : goQuantizer<_input_type, _output_type> ()
{
    setClassName ("goUniformQuantizer");
}

template <class _input_type, class _output_type>
goUniformQuantizer<_input_type, _output_type>::~goUniformQuantizer ()
{
}

template <class _input_type, class _output_type>
_output_type
goUniformQuantizer<_input_type, _output_type>::quantize (_input_type input) const
{
    _output_type output = (_output_type)0;

    return output;
}

template class goUniformQuantizer <goFloat, goInt8>;
