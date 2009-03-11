#ifndef GOGUI_PLOTVIEW_H
#define GOGUI_PLOTVIEW_H

#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif
#include <gtkmm.h>
#include <goplot/graph.h>

namespace goGUI
{
    /** 
     * @brief Plots a goPlot::Graph object.
     */
    class PlotView : public Gtk::DrawingArea
    {
        public:
            PlotView ();
            virtual ~PlotView ();

            /** 
             * @brief Set the graph.
             * 
             * @param g goAutoPtr to a goPlot::Graph object.
             */
            void setGraph (goAutoPtr<goPlot::Graph> g);

        protected:
            virtual bool on_expose_event(GdkEventExpose* event);

        private:
            goAutoPtr<goPlot::Graph> myGraph;
    };
};

#endif
