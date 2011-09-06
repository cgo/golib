/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOPLOT_CAIROPLOT_H
#define GOPLOT_CAIROPLOT_H

#include <goplot/plot.h>
#include <goplot/graph.h>
#ifndef GOSTRING_H
# include <gostring.h>
#endif

namespace goPlot
{
    // template <class Points, class Real>

    /** @addtogroup cairoplot
     * @{
     */
    /** 
     * @brief Plots a graph with cairo.
     */
    class CairoPlot
    {
        public:
            CairoPlot (cairo_t* d, int w, int h, Graph& g)
                : context (d)
            {
                if (g.dim () < 2)
                {
                    return;
                }
                // d->get_size (w, h);
                // printf ("W, H = %d, %d\n", w, h);
                cairo_save (context);
                cairo_matrix_t m;
                // cairo_matrix_init (&m, float(w) / g.axis(0)->length(), 0, 0, -float(h) / g.axis(1)->length(), 0, float(h) / g.axis(1)->length());
                //= FIXME: Use Tics in the graph's axes for hints for the borders.
                //double space_x_0 = 30;
                //double space_y_0 = 20;
                //double space_x_1 = 20;
                //double space_y_1 = 20;
                double space_x_0 = 0;
                double space_y_0 = 0;
                double space_x_1 = 0;
                double space_y_1 = 0;
                cairo_matrix_init (&m, float(w), 0, 0, -float(h), 0, float(h));
                cairo_set_matrix (context, &m);
                g.setContext (context);
                g.getBorderHint (w, h, space_x_0, space_y_0, space_x_1, space_y_1);
                space_x_1 = std::max (space_x_1, 20.0); // FIXME: Funktioniert irgendwie auf der rechten Seite nicht .. ???

                cairo_matrix_init (&m, float(w) - space_x_0 - space_x_1, 0, 0, -float(h) + space_y_1 + space_y_0, space_x_0, float(h) - space_y_0);
                cairo_set_matrix (context, &m);
                //cairo_translate (d, 0.0, 0.5);
                //cairo_scale (d, 0.5, 0.5);
                g.setContext (context);
                g.draw ();
                cairo_restore (context);
            };

            virtual ~CairoPlot () { };

        private:
            cairo_t* context;
    };

    //= Helper functions for quickly plotting stuff
    goAutoPtr<goPlot::Graph> plot (const goMatrixf& curve, goAutoPtr<goPlot::Graph> g = 0);
    goAutoPtr<goPlot::Graph> plot (const goVectorf& x, const goVectorf& y, goAutoPtr<goPlot::Graph> g = 0);
    goAutoPtr<goPlot::Graph> plot (const goMatrixd& curve, goAutoPtr<goPlot::Graph> g = 0);
    goAutoPtr<goPlot::Graph> plot (const goVectord& x, const goVectord& y, goAutoPtr<goPlot::Graph> g = 0);

    bool plot (goAutoPtr<goPlot::Graph> g, const goString& filename, int w = 600, int h = 400);
    void plot (goAutoPtr<goPlot::Graph> g, cairo_t* context, int w = 600, int h = 400);

    //= Moved to goGUI
#if 0
    // template <class Points, class Real>
    /** 
     * @brief Gtkmm widget for plotting. Testing only.
     */
    class CairoPlotWidget : public Gtk::DrawingArea
    {
        public:
            CairoPlotWidget (Graph* g)
                : Gtk::DrawingArea (), myGraph (g)
            {
            };

            virtual ~CairoPlotWidget ()
            {
            };
      
        private:
            Graph* myGraph;

        protected:
            virtual bool on_expose_event(GdkEventExpose* event)
            {
                if (myGraph)
                {
                    int w, h;
                    this->get_window()->get_size (w, h);
                    CairoPlot plot (this->get_window ()->create_cairo_context ()->cobj (), w, h, *myGraph);
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
#endif

    /** @} */

};

#endif
