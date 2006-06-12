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
        this->setClassID(GO_PDF);
    }

    template <class input_type, class output_type>
    goPDF<input_type, output_type>::~goPDF ()
    {
    }
};

template class goMath::goPDF <goDouble, goDouble>;
