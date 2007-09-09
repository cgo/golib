#include <goplot.h>
#include <goautoptr.h>
#include <golist.h>

class goPlot::PlotPrivate
{
    public:
        PlotPrivate () : plots(), plotPositions(), gp(), rows(1), cols(1) {};
        ~PlotPrivate () {};

        goList<goAutoPtr<goSinglePlot> >    plots;
        goList<goVectori> plotPositions;
        goGnuplot         gp;

        goSize_t          rows;
        goSize_t          cols;
};

goPlot::Plot::Plot ()
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new PlotPrivate;
}

goPlot::Plot::~Plot ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goSinglePlot& goPlot::Plot::getPlot (goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    return *pe;
}

goAutoPtr<goSinglePlot> goPlot::Plot::getPlotp (goSize_t x, goSize_t y)
{
    goVectori v(2);
    v[0] = x; v[1] = y;
    goSize_t i = myPrivate->plotPositions.findIndex (v);
    printf ("found index %d\n", i);
    if (i >= myPrivate->plotPositions.getSize())
    {
        if (x >= myPrivate->cols)
            myPrivate->cols = x + 1;
        if (y >= myPrivate->rows)
            myPrivate->rows = y + 1;
        
        myPrivate->plots.append (goAutoPtr<goSinglePlot>(new goSinglePlot));
        printf ("Appended to list\n");
        myPrivate->plotPositions.append (v);

        return myPrivate->plots.getTail();
    }
    else
    {
        return myPrivate->plots(i)->elem;
    }
}

void goPlot::Plot::plot (const goMatrixf& curve, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurveMatrix (curve, title, options);
}

void goPlot::Plot::plot (const goMatrixd& curve, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurveMatrix (curve, title, options);
}

void goPlot::Plot::plot (const goVectorf& px, const goVectorf& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = goAutoPtr<goSinglePlot>(this->getPlotp (x, y));
    printf ("Took plot in plot (...)\n");
    pe->addCurve (px, py, title, options);
}
void goPlot::Plot::plot (const goVectord& px, const goVectord& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = goAutoPtr<goSinglePlot>(this->getPlotp (x, y));
    printf ("Took plot in plot (...)\n");
    pe->addCurve (px, py, title, options);
}
void goPlot::Plot::plot (const goVectorf& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    printf ("Took plot in plot (...)\n");
    pe->addCurve (py, title, options);
}
void goPlot::Plot::plot (const goVectord& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    printf ("Took plot in plot (...)\n");
    pe->addCurve (py, title, options);
}

void goPlot::Plot::plot ()
{
    printf ("Entered plot()\n");
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    goList<goAutoPtr<goSinglePlot> >::Element* pel = myPrivate->plots.getFrontElement();
    goList<goVectori>::Element* cel = myPrivate->plotPositions.getFrontElement();

    while (pel && cel)
    {
        mp.addPlot (*(pel->elem), cel->elem[1], cel->elem[0]);
        pel = pel->next;
        cel = cel->next;
    }

    mp.plot (&myPrivate->gp);
}

void goPlot::Plot::plotPause ()
{
    printf ("Entered plotPause()\n");
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    goList<goAutoPtr<goSinglePlot> >::Element* pel = myPrivate->plots.getFrontElement();
    goList<goVectori>::Element* cel = myPrivate->plotPositions.getFrontElement();

    while (pel && cel)
    {
        mp.addPlot (*(pel->elem), cel->elem[1], cel->elem[0]);
        pel = pel->next;
        cel = cel->next;
    }

    mp.setPauseFlag (true);
    mp.plot ();
}

goGnuplot& goPlot::Plot::getGnuplot ()
{
    return myPrivate->gp;
}
