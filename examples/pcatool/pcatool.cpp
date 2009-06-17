#include <gogui/mainwindow.h>
#include <gogui/control.h>
#include <gogui/helper.h>
#include <gogui/plotview.h>

#include <gtkmm.h>

#include <gofixedarray.h>
#include <gomath.h>
#include <golist.h>
#include <goreparam.h>
#include <gomatrix.h>
#include <govector.h>
#include <gocurve.h>
#include <goplot/graph.h>
#include <goautoptr.h>
#include <goeigenvalue.h>

//= libshape
#include <goshapedistance.h>

class PCATool : public goGUI::MainWindow
{
    public:
        PCATool ();
        virtual ~PCATool ();

        void loadCurves ();

        void pca ();
        void combine (const goVectord& alpha, goMatrixd& ret);

    private:
        goList<goMatrixd> myCurves;
        goGUI::PlotView   myPlotView;
        goAutoPtr<goPlot::Graph> myGraph;
        
        goMatrixd myMean;
        goMatrixd myU;
};

PCATool::PCATool ()
    : goGUI::MainWindow (),
      myCurves (),
      myPlotView (),
      myGraph (0)
{
    Gtk::MenuItem* item = this->addMenuItem (this->getFileMenu(), "Load Curves");
    item->signal_activate().connect (sigc::mem_fun (*this, &PCATool::loadCurves));

    this->getPaned().add1 (myPlotView);

    this->show_all_children ();
}

PCATool::~PCATool ()
{
}

void PCATool::loadCurves ()
{
    static goString last = "";
    goFixedArray<goString> names;
    if (!goGUI::getFilenames (names, last, "Load Curves"))
    {
        return;
    }

    goDouble maxX = 0.0, maxY = 0.0;
    goDouble minX = 0.0, minY = 0.0;
    for (goSize_t i = 0; i < names.getSize(); ++i)
    {
        goCurved c;
        c.readASCII (names[i].toCharPtr());
        goMatrixd M (0, 0), M2 (0, 0);
        this->myCurves.append (M2);
        c.removeDuplicates ();
        c.getConfigurationMatrix (M);
        goMath::Reparam<goDouble>::equidistant (M, 300, true);
        if (!goMath::resampleCubic (M, this->myCurves.getTail (), 100, true))
        {
            goGUI::warning ("Resampling failed.");
            return;
        }

        goVectord t (2);
        goMath::centerOfMass (this->myCurves.getTail(), t);
        goMath::translate (this->myCurves.getTail(), t * -1.0);
        this->myCurves.getTail() /= this->myCurves.getTail().norm ();

        goVectord v (0);
        this->myCurves.getTail ().refColumn (0, v);
        maxX = goMath::max (maxX, goMath::max (v));
        minX = goMath::min (minX, goMath::min (v));
        this->myCurves.getTail ().refColumn (1, v);
        maxY = goMath::max (maxY, goMath::max (v));
        minY = goMath::min (minY, goMath::min (v));
    }

    goAutoPtr<goPlot::Graph> graph = new goPlot::Graph;

    for (goList<goMatrixd>::iterator it = myCurves.begin (); it != myCurves.end (); ++it)
    {
        goAutoPtr<goPlot::Object2DPoints> p = graph->addCurve (*it);
        p->lineTraits().setColour (goPlot::RGBA (0.0, 0.0, 0.0, 0.5));
//        p->setTransform (goPlot::Trafo2D (1.0 / (maxX - minX), 0.0, 0.0, 1.0 / (maxY - minY), -minX / (maxX - minX), -minY / (maxY - minY)));
        // p->setTransform (goPlot::Trafo2D (1.0 / (maxX - minX), 0.0, 0.0, 1.0 / (maxY - minY), -minX, -minY));
        p->setTransform (goPlot::Trafo2D (1.0, 0.0, 0.0, 1.0, 0.5, 0.5));
    }

    graph->disableAxes ();

    myPlotView.setGraph (graph);
    myGraph = graph;

    this->pca ();

    myPlotView.queue_draw ();
}

void PCATool::pca ()
{
    if (myGraph.isNull ())
    {
        return;
    }
        
    goMatrixd& mean = myMean;
    if (!goFullProcrustesMean2DPerm (myCurves, true, mean))
    {
        goGUI::warning ("Procrustes mean failed.");
        return;
    }

    goVectord mean_v (mean.getPtr(), mean.getRows() * 2, 1);
    
    goMatrixd S (0, 0);
    goDouble _N = 1.0 / goDouble (myCurves.getSize());
    for (goList<goMatrixd>::iterator it = myCurves.begin(); it != myCurves.end(); ++it)
    {
        goVectord c_v (it->getPtr(), it->getRows() * 2, 1);
        goMath::vectorOuter (_N, c_v - mean_v, c_v - mean_v, S);
    }

    goMath::Eigenvalue<goDouble> eigen (S);
    eigen.getV (myU);

    printf ("Eigenvalues: ");
    goVectord ev;
    ev = eigen.getRealEigenvalues ();
    for (goSize_t i = 0; i < 5; ++i)
    {
        printf ("%.5lf ", ev[i]);
    }
    printf ("\n");

    goVectord v;
    mean.refColumn (0, v);
    goDouble minX = goMath::min (v);
    goDouble maxX = goMath::max (v);
    mean.refColumn (1, v);
    goDouble minY = goMath::min (v);
    goDouble maxY = goMath::max (v);

    goAutoPtr<goPlot::Object2DPoints> p = myGraph->addCurve (mean);
    p->setTransform (goPlot::Trafo2D (1.0, 0.0, 0.0, 1.0, 0.5, 0.5));
    p->lineTraits ().setColour (goPlot::RGBA (1.0, 0.0, 0.0));
    p->lineTraits ().setWidth (2.0);

    {
        goMatrixd MM;
        goDouble _a[] = {0.0, 0.0, 0.0};
        goVectord alpha (_a, 3, 1);
        this->combine (alpha, MM);

        for (goDouble a = 0.0; a < 0.3; a += 0.03)
        {
            alpha[0] = a;
            this->combine (alpha, MM);
            goAutoPtr<goPlot::Object2DPoints> p = myGraph->addCurve (MM);
            p->setTransform (goPlot::Trafo2D (1.0, 0.0, 0.0, 1.0, 0.5, 0.5));
            p->lineTraits ().setColour (goPlot::RGBA (0.0, 0.0, 1.0));
            p->lineTraits ().setWidth (2.0);
        }
    }

    myPlotView.queue_draw ();
}

void PCATool::combine (const goVectord& alpha, goMatrixd& ret)
{
    const goSize_t N = alpha.getSize ();
    
    ret.resize (myMean.getRows(), 2);
    ret = myMean;

    goVectord r_v (ret.getPtr(), ret.getRows() * 2, 1);

    for (goSize_t i = 0; i < N; ++i)
    {
        goVectord u (0);
        myU.refColumn (i, u);
        r_v += u * alpha[i];
    }
}

int main (int argc, char* argv[])
{
    Gtk::Main kit (argc, argv);

    PCATool window;
    window.show ();

    kit.run (window);

    exit (1);
}
