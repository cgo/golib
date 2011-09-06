/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/goplot.h>
#include <gtkmm.h>
#include <gogui/plotview.h>
#include <gogui/cairoplot.h>

int main (int argc, char* argv[])
{
    const goSize_t N = 100;
    goMatrixd M (N, 2);

    goSize_t i = 0;
    for (goDouble x = 0; x < 2 * M_PI; x += 2 * M_PI / float(N - 1), ++i)
    {
        M (i, 0) = x;
        M (i, 1) = ::sin (x);
    }

    goPlot::plot (goPlot::plot (M), goString ("test.pdf"), 450, 250);
    goPlot::plot (goPlot::plot (M), goString ("test.eps"), 450, 250);
    goPlot::plot (goPlot::plot (M), goString ("test.svg"), 450, 250);
    goGUI::plot (goPlot::plot (M));

    exit (1);

    goPlot::Graph* graph = new goPlot::Graph;
    graph->setDimensions (0.0, 2 * M_PI, -1.0, 1.0);
    // graph->setTransform (goPlot::Trafo2D (1.0, 0.0, 0.0, -1.0, 0.0, 1.0));
    goPlot::Object2DText* t = new goPlot::Object2DText ("You are here!");
    t->setPosition (0.2, ::sin (0.2));
    t->traits().setColour (goPlot::RGBA (1.0, 1.0, 0.0, 1.0));
    graph->add (t);
    graph->addCurve (M);

    Gtk::Main kit (argc, argv);
    goGUI::PlotView pv;
    pv.setGraph (graph);
    Gtk::Window window;
    window.add (pv);
    pv.show ();
    window.show ();
    Gtk::Main::run ();

    exit (1);
}
