#include <gogui/cairoplot.h>
#include <goplot/cairoplot.h>
#include <gogui/plotview.h>
#include <govector.h>

/** 
 * @addtogroup cairoplot
 * @addtogroup gui
 * @brief Creates a Gtkmm window and plots the graph.
 * 
 * @param g Graph to be plotted.
 */
void goGUI::plot (goAutoPtr<goPlot::Graph> g)
{
    char **dummy = 0;
    int argc = 0;
    Gtk::Main kit (argc, (char**&)dummy);
    Gtk::Window window;
    goGUI::PlotView pv;
    pv.setGraph (g);
    window.add (pv);
    window.show ();
    pv.show ();
    Gtk::Main::run ();
}
