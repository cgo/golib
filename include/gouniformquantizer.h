/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOUNIFORMQUANTIZER_H
#define GOUNIFORMQUANTIZER_H

#ifndef GOQUANTIZER_H
# include <goquantizer.h>
#endif
#include <gotypes.h>


template <class _input_type,class _output_type> class goUniformQuantizerPrivate;

/*!
 * \addtogroup signal
 * @{
 */
/**
 * @brief Uniform quantizer.
 *
 * @todo Check the example quantizer.cpp. The last value may not be reached (maybe a rounding problem)
 * and that may also result in the problems with histogram equalisation that have been observed.
 *
 * @param _input_type   Input data type.
 * @param _output_type  Output data type.
 *
 * \author Christian Gosch
 **/
template <class _input_type, class _output_type>
class goUniformQuantizer : public goQuantizer <_input_type, _output_type>
{
    public:
        goUniformQuantizer (goSize_t quantizationSteps = 255);
        goUniformQuantizer (goDouble     delta_input,
                            _input_type  min_input,
                            _input_type  max_input,
                            _output_type min_output,
                            _output_type max_output);
        virtual ~goUniformQuantizer ();

    public:
        virtual _output_type quantize (_input_type) const;

    private:
       goUniformQuantizerPrivate<_input_type, _output_type>* myPrivate; 
        
    private:
        goUniformQuantizer<_input_type, _output_type>& operator= (goUniformQuantizer<_input_type, _output_type>& other);
        goUniformQuantizer (goUniformQuantizer<_input_type, _output_type>& other);
};
/*! @} */
#endif
