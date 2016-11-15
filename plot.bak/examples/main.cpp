/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gtkmm.h>
#include <goplot/cairoplot.h>
#include <goplot/object2dtext.h>
#include <goplot/object2dimage.h>

#include <gogui/cairoplot.h>

#include <gomatrix.h>
#include <gocurve.h>
#include <gosignal.h>
#include <gosignal3d.h>
#include <gosignal3dref.h>
#include <gosignal3dgenericiterator.h>
#include <gofileio.h>

#include <cairo.h>

#if 0
class MyArea : public Gtk::DrawingArea
{
    public:
        MyArea() {};
        virtual ~MyArea() {};

    protected:
        //Override default signal handler:
        virtual bool on_expose_event(GdkEventExpose* event)
        {
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
        };
};
#endif


template <class T>
class MyPoints2D : public NSPACE ::Points2D <T>
{
    public:
        MyPoints2D ()
            : NSPACE ::Points2D <T> (),
              myMatrix (0, 0)
        {
        }

        virtual ~MyPoints2D ()
        {
        }

        MyPoints2D (const goMatrix<T>& M)
            : NSPACE ::Points2D<T> (),
              myMatrix (M)
        {
        }

        MyPoints2D& operator= (const goMatrix<T>& o)
        {
            myMatrix = o;
            return *this;
        }

        virtual T x (int i) const { return myMatrix (i, 0); }
        virtual T y (int i) const { return myMatrix (i, 1); }

        virtual void set (int i, T x, T y)
        {
            myMatrix (i, 0) = x;
            myMatrix (i, 1) = y;
        }
        //= Return number of points
        virtual size_t size () const
        {
            return myMatrix.getRows ();
        }

    private:
        goMatrix<T> myMatrix;


};

typedef NSPACE ::Points2DSimple<double> Points;
typedef double Real;

int main (int argc, char* argv[])
{
    Gtk::Main kit (argc, argv);

    NSPACE ::FontList fl;

    const int N = 100;
    //= Gets deleted by the destructor of Graph. Not good.
    typedef NSPACE ::Object2DPoints <Points, Real> Points_t;
    goAutoPtr<NSPACE ::Object2D> points = new goPlot::Object2DPoints <Points, Real> (N);
    goAutoPtr<NSPACE ::Object2D> points2 = new goPlot::Object2DPoints <Points, Real> (N);

    goAutoPtr<NSPACE ::Object2D> mypoints = new NSPACE ::Object2DPoints <MyPoints2D<goFloat>, goFloat>;
    goMatrix<goFloat> M;
    goCurve<goFloat> c;
    c.readASCII ("/home/christian/Work/shapes/mpeg7-ce1-b/shapes/bat-17.gif.txt");
    c.getConfigurationMatrix (M);
    goVector<goFloat> com;
    goMath::centerOfMass (M, com);
    goMath::translate (M, com * -1.0);
    M *= 3.0 * M_PI / M.norm ();
    com[0] = M_PI; com[1] = 0.0;
    goMath::translate (M, com);
    ((NSPACE ::Object2DPoints <MyPoints2D<goFloat>, goFloat >*)mypoints.get())->points() = M;

    printf ("Size: %ld\n", ((NSPACE ::Object2DPoints <MyPoints2D<goFloat>, goFloat >*)mypoints.get())->points().size ());

    Points_t* p1 = (Points_t*)points.get();
    Points_t* p2 = (Points_t*)points2.get();
    for (int i = 0; i < N; ++i)
    {
        Real t = 2.0 * M_PI / float (N-1) * float(i);
        p1->points().set (i, t, ::sin (t));
        p2->points().set (i, t, ::cos (t));
    }

    p1->lineTraits().setColour (NSPACE ::RGBA (1.0, 0.0, 0.0));
    p2->lineTraits().setColour (NSPACE ::RGBA (0.0, 0.0, 1.0, 0.2));

    NSPACE ::Graph graph;
    graph.add (points);
    graph.add (points2);
    graph.setDimensions (0.0, 2.0 * M_PI, -1.0, 1.0);

    graph.addLabel (M_PI, 0.0, "Hallo, <span bgcolor=\"blue\" fgcolor=\"white\" size=\"larger\">Text!</span>", -0.5, -0.5).traits().setFont ("sans normal 12");

    NSPACE ::Graph *graph2 = new NSPACE ::Graph;
    graph2->add (mypoints);
    goVector<goFloat> v;
    M.refColumn (0, v);
    goFloat minx = goMath::min (v);
    goFloat maxx = goMath::max (v);
    M.refColumn (1, v);
    goFloat miny = goMath::min (v);
    goFloat maxy = goMath::max (v);
    graph2->axis(0)->setLower (minx);
    graph2->axis(0)->setUpper (maxx);
    graph2->axis(1)->setLower (miny);
    graph2->axis(1)->setUpper (maxy);
    graph2->axis(0)->setTics (10);
    graph2->axis(1)->setTics (10);
    goAutoPtr<NSPACE ::Object2D> graph2p (graph2);
    NSPACE ::Trafo2D<NSPACE ::real> trafo2 (M_PI, 0.0, 0.0, 1.0, M_PI, -0.5);
    graph2p->setTransform (trafo2);
    // graph.add (graph2p);

    NSPACE ::Object2DText *txt = new NSPACE ::Object2DText ("1.2345");
    txt->setPosition (M_PI, 0.0);
    // txt->setTransform (NSPACE ::Trafo2D<NSPACE ::real> (1.0, 0.0, 0.0, 1.0, 0.0, 0.0));
    // graph.add (txt);
    // graph.add (NSPACE ::AutoPtr<NSPACE ::Object2D> (txt));

    //= Load image
    goSignal3D<void> sig;
    sig.setDataType (GO_UINT8);
    goFileIO::readImage ("image.png", &sig, true);
    //= Create an image object for the graph
    NSPACE ::Object2DImage *img = new NSPACE ::Object2DImage;
    img->createImage (NSPACE ::Object2DImage::ARGB32, sig.getSizeX (), sig.getSizeY ());

    //= Convert the loaded image from RGB into BGRA in the object2dImage object.
    {
        goSize3D sz (img->stride() / 4, img->height(), 1);
        goSignal3DRef temp (img->data (), GO_UINT8, sz, sz, goSize3D (0, 0, 0), 4);
        goSignal::RGB2BGRA (sig, temp);

        temp.setChannel (3);
        goFillSignal (&temp, 255);
        temp.setChannel (0);
    }
    //= Set transform and add image to graph. Note the data are not residing in sig, but in img (they got copied).
    NSPACE ::Trafo2D<NSPACE ::real> t (1.0/ float (sig.getSizeX()), 0.0, 0.0, -1.0 / float (sig.getSizeY()), 0.0, 1.0);
    img->setTransform (t);
    sig.destroy ();
    graph.add (goAutoPtr<NSPACE ::Object2D> (img));


    goGUI::CairoPlotWidget cp (&graph);

#ifndef CAIRO_HAS_PS_SURFACE
    printf ("Cairo does not have PS!\n");
#endif

    int w = 600; int h = 250;
    {
        cairo_surface_t* s = cairo_ps_surface_create ("test.eps", w, h);
        cairo_ps_surface_set_eps (s, (1 == 1));
        cairo_t* context = cairo_create (s);
        NSPACE ::CairoPlot (context, w, h, graph);
        cairo_surface_finish (s);
        cairo_surface_destroy (s);
    }
    {
        cairo_surface_t* s = cairo_pdf_surface_create ("test.pdf", w, h);
        cairo_t* context = cairo_create (s);
        NSPACE ::CairoPlot (context, w, h, graph);
        cairo_surface_finish (s);
        cairo_surface_destroy (s);
    }
    {
        cairo_surface_t* s = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, w, h);
        cairo_t* context = cairo_create (s);
        cairo_set_source_rgba (context, 1.0, 1.0, 1.0, 1.0);
        cairo_paint (context);
        NSPACE ::CairoPlot (context, w, h, graph);
        cairo_surface_flush (s);
        cairo_surface_write_to_png (s, "test.png");
        cairo_surface_finish (s);
        cairo_surface_destroy (s);
    }
    {
        cairo_surface_t* s = cairo_svg_surface_create ("test.svg", w, h);
        cairo_t* context = cairo_create (s);
        NSPACE ::CairoPlot (context, w, h, graph);
        cairo_surface_finish (s);
        cairo_surface_destroy (s);
    }

    //Gtk::HPaned *pane = Gtk::manage (new Gtk::HPaned);
    //pane->pack1 (cp);
    //pane->pack2 (*Gtk::manage (new Gtk::Button));

    Gtk::Window win;
    win.add (cp);

    cp.show ();
    win.show_all ();
    Gtk::Main::run (win);
}
