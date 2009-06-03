#include <goplot/cairoplot.h>
#include <govector.h>
#include <cairo/cairo.h>
#include <cairo/cairo-pdf.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-svg.h>

template <class T> 
goAutoPtr<goPlot::Graph> plotT (const goVector<T>& x, const goVector<T>& y, goAutoPtr<goPlot::Graph> g)
{
    goAutoPtr<goPlot::Graph> graph = g;
    if (graph.isNull())
    {
        graph = new goPlot::Graph;
    }

    goDouble x0 = g->axis(0)->lower();
    goDouble x1 = g->axis(0)->upper();
    goDouble y0 = g->axis(1)->lower();
    goDouble y1 = g->axis(1)->upper();

    goDouble xmin = goMath::min (x);
    goDouble xmax = goMath::max (x);
    goDouble ymin = goMath::min (y);
    goDouble ymax = goMath::max (y);

    graph->setDimensions (goMath::min (xmin, x0), 
                          goMath::max (xmax, x1), 
                          goMath::min (ymin, y0), 
                          goMath::max (ymax, y1));

    const goSize_t N = x.getSize();

    goAutoPtr<goPlot::Points2DMatrix<T> > points = new goPlot::Points2DMatrix<T> (N);
    
    for (goSize_t i = 0; i < N; ++i)
    {
        points->set (i, x[i], y[i]);
    }

    goAutoPtr<goPlot::Object2DPoints> o = new goPlot::Object2DPoints (points);
    o->lineTraits().setWidth (1.0);

    graph->add (o);
    return graph;
}

template <class T> 
goAutoPtr<goPlot::Graph> plotT (const goMatrix<T>& curve, goAutoPtr<goPlot::Graph> g)
{
    goAutoPtr<goPlot::Graph> graph = g;
    if (graph.isNull())
    {
        graph = new goPlot::Graph;
    }

    const goVector<T> x (0), y (0);
    curve.refColumn (0, x);
    curve.refColumn (1, y);
    return plotT<T> (x, y, graph);
}


/** 
 * @brief Plot a curve given as configuration matrix.
 *
 * The graph will be resized to fit.
 *
 * @param curve Configuration matrix, one point per row.
 * @param g Graph. If Null, a new graph will be created.
 * 
 * @return Pointer to the graph. If \c g was not null, the same as g.
 */
goAutoPtr<goPlot::Graph> goPlot::plot (const goMatrixf& curve, goAutoPtr<goPlot::Graph> g)
{
    return plotT<goFloat> (curve, g);
}

/** 
 * @brief Plot a curve given as configuration matrix.
 * 
 * The graph will be resized to fit.
 *
 * @param curve Configuration matrix, one point per row.
 * @param g Graph. If Null, a new graph will be created.
 * 
 * @return Pointer to the graph. If \c g was not null, the same as g.
 */
goAutoPtr<goPlot::Graph> goPlot::plot (const goMatrixd& curve, goAutoPtr<goPlot::Graph> g)
{
    return plotT<goDouble> (curve, g);
}

/** 
 * @brief Plot a curve given as coordinate vectors.
 * 
 * The graph will be resized to fit.
 *
 * @param x x coordinates of the points.
 * @param y y coordinates of the points.
 * @param g Graph. If Null, a new graph will be created.
 * 
 * @return Pointer to the graph. If \c g was not null, the same as g.
 */
goAutoPtr<goPlot::Graph> goPlot::plot (const goVectorf& x, const goVectorf& y, goAutoPtr<goPlot::Graph> g)
{
    return plotT<goFloat> (x, y, g);
}

/** 
 * @brief Plot a curve given as coordinate vectors.
 * 
 * The graph will be resized to fit.
 *
 * @param x x coordinates of the points.
 * @param y y coordinates of the points.
 * @param g Graph. If Null, a new graph will be created.
 * 
 * @return Pointer to the graph. If \c g was not null, the same as g.
 */
goAutoPtr<goPlot::Graph> goPlot::plot (const goVectord& x, const goVectord& y, goAutoPtr<goPlot::Graph> g)
{
    return plotT<goDouble> (x, y, g);
}

static cairo_surface_t* make_surface (const goString& filename, int w, int h)
{
    if (goString (filename.getPtr() + filename.getSize() - 4) == ".pdf")
    {
        cairo_surface_t* s = ::cairo_pdf_surface_create (filename.toCharPtr(), w, h);
        return s;
    }
    if (goString (filename.getPtr() + filename.getSize() - 4) == ".eps")
    {
        cairo_surface_t* s = ::cairo_ps_surface_create (filename.toCharPtr(), w, h); 
        ::cairo_ps_surface_set_eps (s, (1 == 1));
        return s;
    }
    if (goString (filename.getPtr() + filename.getSize() - 4) == ".svg")
    {
        cairo_surface_t* s = ::cairo_svg_surface_create (filename.toCharPtr(), w, h);
        return s;
    }

    return 0;
}

/** 
 * @brief Plot a graph to a file.
 *
 * Currently, PDF, EPS and SVG files are supported through the respective functions of Cairo.
 * 
 * @param g Graph to plot.
 * @param filename Filename. The ending determines the format: one of .pdf, .eps, .svg.
 * @param w Width of the result in points
 * @param h Height of the result in points
 * 
 * @return True if successful, false otherwise.
 */
bool goPlot::plot (goAutoPtr<goPlot::Graph> g, const goString& filename, int w, int h)
{
    cairo_surface_t* s = make_surface (filename, w, h);
    
    if (!s)
        return false;

    cairo_t* context = cairo_create (s);

    if (!context)
        return false;
            
    plot (g, context, w, h);

    ::cairo_surface_finish (s);
    ::cairo_surface_destroy (s);
    ::cairo_destroy (context);

    return true;
}

/** 
 * @brief Plot to a given Cairo context.
 *
 * Use the other goPlot::plot function to plot directly to files in a more convenient way.
 * 
 * @param g Graph to plot.
 * @param context Cairo context to plot to.
 * @param w Width of the graph in points.
 * @param h Height of the graph in points.
 */
void goPlot::plot (goAutoPtr<goPlot::Graph> g, cairo_t* context, int w, int h)
{
    CairoPlot p (context, w, h, *g);
}
