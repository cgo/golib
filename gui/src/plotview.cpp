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

bool goGUI::PlotView::on_expose_event (GdkEventExpose* event)
{
    if (!myGraph.isNull ())
    {
        int w, h;
        this->get_window()->get_size (w, h);
        goPlot::CairoPlot plot (this->get_window ()->create_cairo_context ()->cobj (), w, h, *myGraph);
    }

    return true;
}
