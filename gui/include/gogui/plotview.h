/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
            virtual bool on_draw(Cairo::RefPtr<Cairo::Context> const& context) override;

        private:
            goAutoPtr<goPlot::Graph> myGraph;
    };
};

#endif
