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


class PCAControl : public goGUI::Control
{
    public:
        PCAControl ();
        virtual ~PCAControl ();

        void setComponentsCount (int n);
        void setRange (int idx, double min, double max);
        goCaller1 <void, const goVectord&>& getCaller ();

        void valueChangedSlot ();

    private:
        std::vector<Gtk::HScale*> myScales;
        Gtk::VBox myBox;
        goCaller1 <void, const goVectord&> myValueChangedCaller;
};

PCAControl::PCAControl ()
    : goGUI::Control ("PCA Control"),
      myScales (),
      myBox ()
{
    this->add (myBox);
}

PCAControl::~PCAControl ()
{
}

void PCAControl::setComponentsCount (int n)
{
    if (n == myScales.size ())
    {
        return;
    }

    int n_old = (int)myScales.size ();

    myScales.resize (n);
    for (int i = n_old; i < n; ++i)
    {
        myScales[i] = Gtk::manage (new Gtk::HScale);
        myScales[i]->set_range (0.0, 1.0);
        myScales[i]->set_increments (0.001, 0.01);
        myScales[i]->set_digits (4);
        myBox.pack_start (*myScales[i], Gtk::PACK_SHRINK);
        myScales[i]->signal_value_changed ().connect (sigc::mem_fun (*this, &PCAControl::valueChangedSlot));
        myScales[i]->show ();
    }
}

void PCAControl::setRange (int idx, double min, double max)
{
    if (idx >= 0 && idx < myScales.size ())
    {
        myScales[idx]->set_range (min, max);
    }
}

void PCAControl::valueChangedSlot ()
{
    goVectord v (myScales.size ());
    for (size_t i = 0; i < myScales.size(); ++i)
    {
        v[i] = myScales[i]->get_value ();
    }
    myValueChangedCaller (v);
}

goCaller1 <void, const goVectord&>& PCAControl::getCaller ()
{
    return myValueChangedCaller;
}

//====================================================================

class PCATool : public goGUI::MainWindow
{
    public:
        PCATool ();
        virtual ~PCATool ();

        void loadCurves ();

        void pca ();
        void combine (const goVectord& alpha, goMatrixd& ret);

        void drawCombination (const goVectord& alpha);

    private:
        goList<goMatrixd> myCurves;
        goGUI::PlotView   myPlotView;
        goAutoPtr<goPlot::Graph> myGraph;
        
        goMatrixd myMean;
        goMatrixd myU;

        goAutoPtr<goPlot::Object2DPoints> myPCACurve;
        PCAControl* myPCAControl;
};

PCATool::PCATool ()
    : goGUI::MainWindow (),
      myCurves (),
      myPlotView (),
      myGraph (0),
      myMean (),
      myU (),
      myPCACurve (0),
      myPCAControl (0)
{
    Gtk::MenuItem* item = this->addMenuItem (this->getFileMenu(), "Load Curves");
    item->signal_activate().connect (sigc::mem_fun (*this, &PCATool::loadCurves));
    this->addFileQuit ();

    PCAControl* control = Gtk::manage (new PCAControl);
    control->setComponentsCount (3);
    control->getCaller().connect (goMemberFunction<void, PCATool, const goVectord&> (this, &PCATool::drawCombination));

    myPCAControl = control;

    this->addControl (*control);

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

    myPCACurve = 0;
    myCurves.erase ();

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
        p->setTransform (goPlot::Trafo2D (1.0 / (maxX - minX) * 0.2, 0.0, 0.0, 1.0 / (maxY - minY) * 0.2, 0.2, 0.8));
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

    //= FIXME: Calculate alpha = U_k^T * (u - \mu) for all curves u and use the extrema as ranges.
    {
        int N = 3;
        myPCAControl->setComponentsCount (N);
        for (int i = 0; i < N; ++i)
        {
            myPCAControl->setRange (i, -6.0 * ev[i], 6.0 * ev[i]);
        }
    }

    goVectord v;
    mean.refColumn (0, v);
    goDouble minX = goMath::min (v);
    goDouble maxX = goMath::max (v);
    mean.refColumn (1, v);
    goDouble minY = goMath::min (v);
    goDouble maxY = goMath::max (v);

    goAutoPtr<goPlot::Object2DPoints> p = myGraph->addCurve (mean);
    p->setTransform (goPlot::Trafo2D (1.0 / (maxX - minX) * 0.2, 0.0, 0.0, 1.0 / (maxY - minY) * 0.2, 0.2, 0.8));
    p->lineTraits ().setColour (goPlot::RGBA (1.0, 0.0, 0.0));
    p->lineTraits ().setWidth (2.0);

    myPlotView.queue_draw ();
}

void PCATool::combine (const goVectord& alpha, goMatrixd& ret)
{
    const goSize_t N = alpha.getSize ();
    
    if (N >= myU.getColumns())
    {
        return;
    }

    if (ret.getRows() != myMean.getRows() || ret.getColumns() != 2)
    {
        ret.resize (myMean.getRows(), 2);
    }
    ret = myMean;

    goVectord r_v (ret.getPtr(), ret.getRows() * 2, 1);

    for (goSize_t i = 0; i < N; ++i)
    {
        goVectord u (0);
        myU.refColumn (i, u);
        r_v += u * alpha[i];
    }
}

void PCATool::drawCombination (const goVectord& alpha)
{
    static goMatrixd MM;

    if (myGraph.isNull())
    {
        return;
    }

    this->combine (alpha, MM);

    if (!myPCACurve.isNull())
    {
        myGraph->remove (myPCACurve);
        myPCACurve = 0;
    }

    goVectord v (0);
    MM.refColumn (0, v);
    goDouble minX = goMath::min (v);
    goDouble maxX = goMath::max (v);
    MM.refColumn (1, v);
    goDouble minY = goMath::min (v);
    goDouble maxY = goMath::max (v);

    myPCACurve = myGraph->addCurve (MM);
    myPCACurve->setTransform (goPlot::Trafo2D (1.0 / (maxX - minX) * 0.8, 0.0, 0.0, 1.0 / (maxY - minY) * 0.8, 0.5, 0.5));
    myPCACurve->lineTraits ().setColour (goPlot::RGBA (0.0, 0.0, 1.0));
    myPCACurve->lineTraits ().setWidth (2.0);

    this->myPlotView.queue_draw ();
}

int main (int argc, char* argv[])
{
    Gtk::Main kit (argc, argv);

    PCATool window;
    window.show ();

    kit.run (window);

    exit (1);
}
