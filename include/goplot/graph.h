/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOPLOT_GRAPH_H
#define GOPLOT_GRAPH_H

#include <goautoptr.h>

#include <goplot/plot.h>
#include <goplot/graphaxis.h>
#include <goplot/autoptr.h>
#include <goplot/object2dimage.h>

#include <exception>
#include <list>
#include <vector>
#include <algorithm>

#include <assert.h>

#ifndef GOSIGNAL_H
# include <gosignal.h>
#endif
#ifndef GOSIGNAL3DREF_H
# include <gosignal3dref.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goPlot
{
    /** @addtogroup cairoplot
     * @{
     */
    /** 
     * @brief Graph. Contains multiple Object2D and some axes to be drawn.
     * Can also contain other Graphs.
     */
    class Graph : public Object2D
    {
        public:
            /** 
             * @brief Construct a graph (axes).
             * 
             * @param dim Dimension -- currently, only 2 is supported.
             */
            Graph (int dim = 2);

            Graph (const Graph& other);

            Graph& operator= (const Graph& other);

            virtual ~Graph ();

            /** 
             * @brief Disables all axes.
             *
             * This can e.g. be used if you want to use this graph as an image display.
             */
            void disableAxes ();

            /** 
             * @brief Multiply the current transformation so that
             * the Y axis is mirrored.
             *
             * Useful if displaying an image which starts in the upper left corner.
             */
            void flipY ();

            /** 
             * @brief Removes all objects.
             */
            void clear ();

            //================================================================================
            //=
            //= Convenience functions for adding new objects.
            //=

            goAutoPtr<goPlot::Object2DPoints> addCurve (const goMatrixd& c);

            goAutoPtr<goPlot::Object2DImage> makeImage (int width, int height, int format = goPlot::Object2DImage::RGB24);

            goAutoPtr<goPlot::Object2DImage> addImage (const goSignal3DBase<void>& image);

            //=
            //= End convenience functions.
            //================================================================================

            void setDimensions (goPlot::real xmin, goPlot::real xmax, goPlot::real ymin, goPlot::real ymax);


            /** 
             * @brief Set the font for the tics text.
             *
             * Convenience function that sets the font of all axes to \c f.
             * @see GraphAxis::setTicsFont().
             *
             * @param f Font description text.
             */
            void setTicsFont (const char* f);

            int              dim () const;
            goAutoPtr<goPlot::GraphAxis>  axis (int i);

            const goAutoPtr<goPlot::GraphAxis> axis (int i) const;

            /** 
             * @brief Adds a Object2D to this graph.
             * 
             * @param o The object pointer (managed by a goAutoPtr)
             */
            void add (goAutoPtr<goPlot::Object2D> o);

            /** 
            * @brief Removes all occurrences of \c o in this graph.
            * 
            * @param o Object pointer to remove.
            */
            void remove (goAutoPtr<goPlot::Object2D> o);

            /** 
             * @brief Sets the title of the graph.
             *
             * The title is not drawn by the Graph::draw() method. 
             * It is only stored here and should be drawn by the class arranging and plotting the graphs.
             * 
             * @param t Title
             */
            void setTitle (const char* t);

            /** 
             * @brief Get the title text.
             * @see setTitle()
             * 
             * @return The title text
             */
            const char* title () const;

            /** 
             * @brief Adds a text label.
             *
             * Convenience function that creates a Object2DText and
             * calls add().
             *
             * \c rel_x and \c rel_y can be used to align the label (see Object2DText::setPosition()).
             *
             * @param x X position
             * @param y Y position
             * @param label Label text
             * @param rel_x Relative x movement in units of text width
             * @param rel_y Relative y movement in units of text height
             *
             * @return Reference to the new Object2DText
             */
            goPlot::Object2DText& addLabel (double x, double y, const char* label, double rel_x = 0.0, double rel_y = 0.0);

            /** 
             * @brief Sets the context and hands it on to all axes and drawable objects.
             * 
             * @param c The context
             */
            virtual void setContext (cairo_t* c);

            void applyAxesTransform (cairo_t* c);

            /** 
             * @brief Draws axes (according to their visibilities) and all objects.
             */
            virtual void draw ();

            void getBorderHint (double w, double h, double& x1, double& y1, double& x2, double& y2);

        protected:
            std::list<goAutoPtr<goPlot::Object2D> > objects2D () { return myObjects; }

        private:
            std::vector<goAutoPtr<goPlot::GraphAxis> > myAxes;
            std::list<goAutoPtr<goPlot::Object2D> >    myObjects;
            int                              myDim;
            std::string                      myTitle;
    };
    /** @} */
};

#endif
