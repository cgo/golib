/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOQUANTIZER_H
#define GOQUANTIZER_H

#include <goobjectbase.h>

/*!
 * \addtogroup signal
 * @{
 */
/**
 * @brief Quantizer base class.
 *
 * Derive from this class for quantizers.
 *
 * \author Christian Gosch
 **/
template <class _input_type, class _output_type>
class goQuantizer : public goObjectBase
{
    public:
        goQuantizer(); 
        virtual~ goQuantizer();

    public:
        virtual _output_type quantize (_input_type) const;

    private:
        goQuantizer (goQuantizer<_input_type, _output_type>& other);
        goQuantizer<_input_type, _output_type>& operator= (goQuantizer<_input_type, _output_type>& other); 
};
/*! @} */
#endif
