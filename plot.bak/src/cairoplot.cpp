/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/cairoplot.h>

namespace NSPACE
{
    class CairoPlotPrivate
    {
        public:
            CairoPlotPrivate () {};
            ~CairoPlotPrivate () {};

            Cairo::RefPtr<Cairo::Context> context;
    };
};

NSPACE ::CairoPlot::CairoPlot (Cairo::RefPtr<Cairo::Context> context)
    : myPrivate (0)
{
    myPrivate = new CairoPlotPrivate;
    myPrivate->context = context;
}

NSPACE ::CairoPlot::~CairoPlot ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

void NSPACE ::CairoPlot::test ()
{
    myPrivate->context->move_to (0, 0);
    myPrivate->context->line_to (1, 1);
    myPrivate->context->set_line_width (0.05);
    myPrivate->context->set_source_rgba (1.0, 0.0, 0.0, 0.2);
    myPrivate->context->stroke ();
    myPrivate->context->move_to (1, 0);
    myPrivate->context->line_to (0, 1);
    myPrivate->context->set_source_rgba (0.0, 1.0, 0.0, 0.2);
    myPrivate->context->stroke ();
}
