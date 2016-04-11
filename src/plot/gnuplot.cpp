/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/gnuplot.h>
#include <goautoptr.h>
#include <golist.h>

class goPlot::PlotPrivate
{
    public:
        PlotPrivate () : plots(), plotPositions(), gp(), rows(1), cols(1), prefix (""), postfix (""), title ("") {};
        ~PlotPrivate () {};

        goList<goAutoPtr<goSinglePlot> >    plots;
        goList<goMath::Vectori> plotPositions;
        goGnuplot         gp;

        goSize_t          rows;
        goSize_t          cols;

        goString          prefix;
        goString          postfix;
        goString          title;
};

goPlot::Gnuplot::Gnuplot ()
    : goObjectBase (),
      myPrivate (0)
{
    myPrivate = new PlotPrivate;
}

goPlot::Gnuplot::~Gnuplot ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goSinglePlot& goPlot::Gnuplot::getPlot (goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    return *pe;
}

goAutoPtr<goSinglePlot> goPlot::Gnuplot::getPlotp (goSize_t x, goSize_t y)
{
    goMath::Vectori v(2);
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

void goPlot::Gnuplot::plot (const goMath::Matrixf& curve, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurveMatrix (curve, title, options);
}

void goPlot::Gnuplot::plot (const goMath::Matrixd& curve, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurveMatrix (curve, title, options);
}

void goPlot::Gnuplot::plot (const goMath::Vectorf& px, const goMath::Vectorf& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = goAutoPtr<goSinglePlot>(this->getPlotp (x, y));
    pe->addCurve (px, py, title, options);
}
void goPlot::Gnuplot::plot (const goMath::Vectord& px, const goMath::Vectord& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = goAutoPtr<goSinglePlot>(this->getPlotp (x, y));
    pe->addCurve (px, py, title, options);
}
void goPlot::Gnuplot::plot (const goMath::Vectorf& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurve (py, title, options);
}
void goPlot::Gnuplot::plot (const goMath::Vectord& py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addCurve (py, title, options);
}

void goPlot::Gnuplot::plotImage (const goSignal3DBase<void>& image, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addImage (image, title, options);
}

void goPlot::Gnuplot::plotImage (const goMath::Matrixf& image, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addImage (image, title, options);
}

void goPlot::Gnuplot::plotImage (const goMath::Matrixd& image, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addImage (image, title, options);
}

void goPlot::Gnuplot::plotPoint (goDouble px, goDouble py, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    goMath::Vectord p (2);
    p[0] = px;
    p[1] = py;
    pe->addPoint (p, title, options);
}

void goPlot::Gnuplot::plotPoint (const goMath::Vectorf& p, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addPoint (p, title, options);
}

void goPlot::Gnuplot::plotPoint (const goMath::Vectord& p, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addPoint (p, title, options);
}

void goPlot::Gnuplot::plotLine  (const goMath::Vectorf& n, const goMath::Vectorf& p, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addLine (n, p, title, options);
}

void goPlot::Gnuplot::plotLine  (const goMath::Vectord& n, const goMath::Vectord& p, const char* title, const char* options, goSize_t x, goSize_t y)
{
    goAutoPtr<goSinglePlot> pe = this->getPlotp (x, y);
    pe->addLine (n, p, title, options);
}

void goPlot::Gnuplot::plot (goMultiPlotter& mp)
{
    goList<goAutoPtr<goSinglePlot> >::Element* pel = myPrivate->plots.getFrontElement();
    goList<goMath::Vectori>::Element* cel = myPrivate->plotPositions.getFrontElement();

    while (pel && cel)
    {
        mp.addPlot (*(pel->elem), cel->elem[1], cel->elem[0]);
        pel = pel->next;
        cel = cel->next;
    }

    if (myPrivate->prefix != "")
        mp.setPrefix (myPrivate->prefix);
    if (myPrivate->postfix != "")
        mp.setPostfix (myPrivate->postfix);
    if (myPrivate->title != "")
        mp.setTitle (myPrivate->title.toCharPtr());
}

void goPlot::Gnuplot::plot ()
{
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    this->plot (mp);
    mp.plot (&myPrivate->gp);
}

void goPlot::Gnuplot::plotPostscript (const char* filename, goFloat sizeX, goFloat sizeY)
{
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    this->plot (mp);
    mp.plotPostscript (filename, sizeX, sizeY);
}

void goPlot::Gnuplot::plotFile (const char* filename, const char* termstring)
{
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    this->plot (mp);
    mp.plotFile (filename, termstring);
}

void goPlot::Gnuplot::plotPause ()
{
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    this->plot (mp);
    mp.setPauseFlag (true);
    mp.plot ();
}

void goPlot::Gnuplot::saveGnuplot (const char* filename)
{
    goMultiPlotter mp (myPrivate->rows, myPrivate->cols);
    goList<goAutoPtr<goSinglePlot> >::Element* pel = myPrivate->plots.getFrontElement();
    goList<goMath::Vectori>::Element* cel = myPrivate->plotPositions.getFrontElement();

    while (pel && cel)
    {
        mp.addPlot (*(pel->elem), cel->elem[1], cel->elem[0]);
        pel = pel->next;
        cel = cel->next;
    }

    if (myPrivate->prefix != "")
        mp.setPrefix (myPrivate->prefix);
    if (myPrivate->postfix != "")
        mp.setPostfix (myPrivate->postfix);
    mp.saveGnuplot (filename);
}

void goPlot::Gnuplot::setPrefix (const char* p)
{
    myPrivate->prefix = p;
}

void goPlot::Gnuplot::addPrefix (const char* p)
{
    myPrivate->prefix += p;
}

void goPlot::Gnuplot::setPostfix (const char* p)
{
    myPrivate->postfix = p;
}

void goPlot::Gnuplot::addPostfix (const char* p)
{
    myPrivate->postfix += p;
}

void goPlot::Gnuplot::setTitle (const char* s)
{
    myPrivate->title = s;
}

/** 
 * @brief Removes all plots.
 */
void goPlot::Gnuplot::clear ()
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
void goPlot::Gnuplot::clear (goSize_t x, goSize_t y)
{
    goMath::Vectori temp(2);
    temp[0] = x; temp[1] = y;
    if (!myPrivate->plotPositions.contains(temp))
        return;
    this->getPlot(x,y).clear ();
}

goGnuplot& goPlot::Gnuplot::getGnuplot ()
{
    return myPrivate->gp;
}


