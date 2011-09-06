/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GO_ADAPTIVEQUANTIZER_H
#define GO_ADAPTIVEQUANTIZER_H

#include <gotypes.h>
#include <goquantizer.h>

template <class _input_type, class _output_type> class _goAdaptiveQuantizer;

template <class _input_type, class _output_type>
class goAdaptiveQuantizer : public goQuantizer
{
    public:
        goAdaptiveQuantizer ();
        virtual ~goAdaptiveQuantizer ();

    public:
        virtual _output_type quantize (_input_type) const;

    private:
        _goAdaptiveQuantizer* myPrivate;
};

#endif
