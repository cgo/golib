#ifndef GOGUI_CAIROPLOT_H
#define GOGUI_CAIROPLOT_H

#include <goplot/cairoplot.h>
#include <gtkmm.h>

namespace goGUI 
{
    // template <class Points, class Real>
    /** 
     * @brief Gtkmm widget for plotting with goplot. 
     */
    class CairoPlotWidget : public Gtk::DrawingArea
    {
        public:
            CairoPlotWidget (goPlot::Graph* g)
                : Gtk::DrawingArea (), myGraph (g)
            {
            };

            virtual ~CairoPlotWidget ()
            {
            };
      
        private:
            goPlot::Graph* myGraph;

        protected:
            virtual bool on_expose_event(GdkEventExpose* event)
            {
                if (myGraph)
                {
                    int w, h;
                    this->get_window()->get_size (w, h);
                    goPlot::CairoPlot plot (this->get_window ()->create_cairo_context ()->cobj (), w, h, *myGraph);
                }

                return true;
#if 0
                Cairo::RefPtr<Cairo::Context> context = this->get_window()->create_cairo_context ();
                NSPACE ::CairoPlot plot (context);
                Gtk::Allocation allocation = get_allocation();
                const int width = allocation.get_width();
                const int height = allocation.get_height();

                Cairo::Matrix m; // (double(width), 0.0, 0.0, double(height), 0.0, 0.0);
                // m.scale (width, height);
                // m.translate (0.0, 0.0);
                // Ist ein typedef auf cairo_matrix_t!
                cairo_matrix_init (&m, width, 0, 0, height, 0, 0);
                context->set_matrix (m);
                plot.test ();
#endif
            };
    };

    //= Helper functions for quickly plotting stuff -- use e.g. with goPlot::plot functions in goplot/cairoplot.h
    void plot (goAutoPtr<goPlot::Graph> g);
};

#endif
