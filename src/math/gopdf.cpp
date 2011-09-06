/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gopdf.h>
#include <godefs.h>
#ifndef GOTYPES_H
# include <gotypes.h>
#endif

namespace goMath
{
    template <class input_type, class output_type>
    goPDF<input_type, output_type>::goPDF ()
        : goObjectBase ()
    {
        
    }

    template <class input_type, class output_type>
    goPDF<input_type, output_type>::~goPDF ()
    {
    }
};

template class goMath::goPDF <goDouble, goDouble>;
