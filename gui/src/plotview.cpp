/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gogui/plotview.h>
#include <goplot/cairoplot.h>

goGUI::PlotView::PlotView ()
    : Gtk::DrawingArea (), myGraph (0)
{
}

goGUI::PlotView::~PlotView ()
{
}

void goGUI::PlotView::setGraph (goAutoPtr<goPlot::Graph> g)
{
    myGraph = g;

    // printf ("PlotView::setGraph(): graph trafo: %f %f %f %f\n", myGraph->transform().xx, myGraph->transform().xy, myGraph->transform().yx, myGraph->transform().yy);
}

bool goGUI::PlotView::on_draw (Cairo::RefPtr<Cairo::Context> const& context)
{
    if (!myGraph.isNull ())
    {
        int x, y, w, h;
        this->get_window()->get_geometry (x, y, w, h);
        goPlot::plot (myGraph, this->get_window()->create_cairo_context()->cobj(), w, h);
        // goPlot::CairoPlot plot (this->get_window ()->create_cairo_context ()->cobj (), w, h, *myGraph);
//        goPlot::CairoPlot plot (context->cobj (), w, h, *myGraph);
    }

    return true;
}
