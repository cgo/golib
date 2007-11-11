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
    if (i >= myPrivate->plotPositions.getSize())
    {
        if (x >= myPrivate->cols)
            myPrivate->cols = x + 1;
        if (y >= myPrivate->rows)
            myPrivate->rows = y + 1;
        
        myPrivate->plots.append (goAutoPtr<goSinglePlot>(new goSinglePlot));
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
    pe->addCurve (px, py, title, options);
}
void goPlot::Plot::plot (const goVectord& px, const goVectord& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = goAutoPtr<goSinglePlot>(this->getPlotp (x, y));
    pe->addCurve (px, py, title, options);
}
void goPlot::Plot::plot (const goVectorf& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurve (py, title, options);
}
void goPlot::Plot::plot (const goVectord& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurve (py, title, options);
}

void goPlot::Plot::plotImage (const goSignal3DBase<void>& image, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addImage (image, title, options);
}

void goPlot::Plot::plotPoint (goDouble px, goDouble py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    goVectord p (2);
    p[0] = px;
    p[1] = py;
    pe->addPoint (p, title, options);
}

void goPlot::Plot::plotPoint (const goVectorf& p, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addPoint (p, title, options);
}

void goPlot::Plot::plotPoint (const goVectord& p, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addPoint (p, title, options);
}

void goPlot::Plot::plot ()
{
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

void goPlot::Plot::plotPostscript (const char* filename, goFloat sizeX, goFloat sizeY)
{
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    goList<goAutoPtr<goSinglePlot> >::Element* pel = myPrivate->plots.getFrontElement();
    goList<goVectori>::Element* cel = myPrivate->plotPositions.getFrontElement();

    while (pel && cel)
    {
        mp.addPlot (*(pel->elem), cel->elem[1], cel->elem[0]);
        pel = pel->next;
        cel = cel->next;
    }

    mp.plotPostscript (filename, sizeX, sizeY);
}

void goPlot::Plot::plotPause ()
{
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

void goPlot::Plot::saveGnuplot (const char* filename)
{
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    goList<goAutoPtr<goSinglePlot> >::Element* pel = myPrivate->plots.getFrontElement();
    goList<goVectori>::Element* cel = myPrivate->plotPositions.getFrontElement();

    while (pel && cel)
    {
        mp.addPlot (*(pel->elem), cel->elem[1], cel->elem[0]);
        pel = pel->next;
        cel = cel->next;
    }

    mp.saveGnuplot (filename);
}

/** 
 * @brief Removes all plots.
 */
void goPlot::Plot::clear ()
{
    myPrivate->plots.erase ();
    myPrivate->plotPositions.erase ();
//    goList<goAutoPtr<goSinglePlot> >::Element* pel = myPrivate->plots.getFrontElement();

//    while (pel)
//    {
//        pel->elem.clear ();
//        pel = pel->next;
//    }
}

/** 
 * @brief Clears plot (x,y), if it exists.
 *
 * If the plot does not exist, does nothing.
 * 
 * @param x x position of the plot.
 * @param y y position of the plot.
 */
void goPlot::Plot::clear (goSize_t x, goSize_t y)
{
    goVectori temp(2);
    temp[0] = x; temp[1] = y;
    if (!myPrivate->plotPositions.contains(temp))
        return;
    this->getPlot(x,y).clear ();
}

goGnuplot& goPlot::Plot::getGnuplot ()
{
    return myPrivate->gp;
}
