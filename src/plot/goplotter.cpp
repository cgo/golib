/*
 * goplotter.cpp
 *
 *  Created on: Apr 10, 2016
 *      Author: christian
 */

#include <goplot/gnuplot.h>

class goPlotterPrivate
{
    public:
        goPlotterPrivate() : plotX(), plotY(), titles(), plotCommands(), prefixCommands(""), shellPostfix(""),
                             waitFlag(true), pauseFlag(false), cmdFilename(""), dataFilenames() {};
        ~goPlotterPrivate() {};

        goList<goMath::Vectord>      plotX;
        goList<goMath::Vectord>      plotY;
        goList<goString>       titles;
        goList<goPlotterLabel> labels;
        goList<goString>       plotCommands;
        goString               prefixCommands;
        goString               shellPostfix;

        bool              waitFlag;
        bool              pauseFlag;
        goString          cmdFilename;
        goList<goString>  dataFilenames;
};

goPlotter::goPlotter ()
    :  goObjectBase (),
       myPrivate (0)
{
    this->setClassID(GO_PLOTTER);
    myPrivate = new goPlotterPrivate;
}

goPlotter::~goPlotter ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

goPlotter::goPlotter (const goPlotter& other)
{
    this->setClassID(GO_PLOTTER);
    myPrivate = new goPlotterPrivate;
    *this = other;
}


const goPlotter& goPlotter::operator= (const goPlotter& other)
{
    *myPrivate = *other.myPrivate;
    return *this;
}

bool goPlotter::addCurve (const goMath::Vectord& x, const goMath::Vectord& y, const char* title, const char* plotOptions)
{
    if (x.getSize() != y.getSize())
    {
        goLog::warning("addCurve(): x and y array sizes mismatch.",this);
        return false;
    }
    myPrivate->plotX.append(x);
    myPrivate->plotY.append(y);
    myPrivate->titles.append(goString(title));
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
    }
    return true;
}

#if 0
bool goPlotter::addCurve (const goMath::Vectorf& x, const goMath::Vectorf& y, const char* title, const char* plotOptions)
{
    if (x.getSize() != y.getSize())
    {
        goLog::warning("addCurve(): x and y array sizes mismatch.",this);
        return false;
    }
    goMath::Vectord xx;
    xx = x;
    goMath::Vectord yy;
    yy = y;
    myPrivate->plotX.append(xx);
    myPrivate->plotY.append(yy);
    myPrivate->titles.append(goString(title));
    if (!plotOptions)
    {
        myPrivate->plotCommands.append(goString("w l"));
    }
    else
    {
        myPrivate->plotCommands.append(goString(plotOptions));
    }
    return true;
}
#endif

bool goPlotter::addLabel (const goString& l, goDouble x, goDouble y)
{
    return myPrivate->labels.append (goPlotterLabel(l.toCharPtr(), x, y));
}

void goPlotter::setWaitFlag (bool w)
{
    myPrivate->waitFlag = w;
}

bool goPlotter::getWaitFlag () const
{
    return myPrivate->waitFlag;
}

void goPlotter::setPauseFlag (bool w)
{
    myPrivate->pauseFlag = w;
}

bool goPlotter::getPauseFlag () const
{
    return myPrivate->pauseFlag;
}

bool goPlotter::plot ()
{
    goString prefix = myPrivate->prefixCommands;
    if (!myPrivate->labels.isEmpty())
    {
        assert (!myPrivate->labels.isClosed());
        goList<goPlotterLabel>::Element* el = myPrivate->labels.getFrontElement();
        while (el)
        {
            prefix += "set label \"";
            prefix += el->elem.label;
            prefix += "\" at ";
            prefix += (float)el->elem.x;
            prefix += ",";
            prefix += (float)el->elem.y;
            prefix += "\n";
            el = el->next;
        }
        // prefix += "replot\n";
    }
    goString postfix = "";
    if (myPrivate->pauseFlag)
    {
        // postfix += "pause -1\n";
        postfix += "pause mouse\n";
    }
    if (!goPlot::writeGnuplotDataFiles (&myPrivate->plotX,
                                        &myPrivate->plotY,
                                        myPrivate->dataFilenames))
    {
        return false;
    }
    goString gnuplotCommands = "";
    if (!goPlot::addGnuplotCommands (gnuplotCommands,
                                     &myPrivate->dataFilenames,
                                     &myPrivate->titles,
                                     &myPrivate->plotCommands,
                                     prefix.toCharPtr(),
                                     postfix.toCharPtr()))
    {
        return false;
    }
    return goPlot::callGnuplot (gnuplotCommands,
                                myPrivate->shellPostfix != "" ? myPrivate->shellPostfix.toCharPtr() : 0,
                                myPrivate->waitFlag,
                                &myPrivate->cmdFilename);

#if 0
    return goPlot::gnuplotList (&myPrivate->plotX,
                                &myPrivate->plotY,
                                &myPrivate->titles,
                                &myPrivate->plotCommands,
                                prefix.toCharPtr(),
                                // myPrivate->prefixCommands.toCharPtr(),
                                postfix.toCharPtr(),
                                myPrivate->shellPostfix != "" ? myPrivate->shellPostfix.toCharPtr() : 0,
                                myPrivate->waitFlag ? 0 : &myPrivate->cmdFilename,
                                myPrivate->waitFlag ? 0 : &myPrivate->dataFilenames,
                                myPrivate->waitFlag);
#endif
}

bool goPlotter::plotPostscript (const goString& filename)
{
    return this->plotFile (filename, goString("postscript color"));
}

bool goPlotter::plotFile (const goString& filename, const goString& type)
{
    goString backup1 = myPrivate->prefixCommands;
    myPrivate->prefixCommands = "set terminal ";
    myPrivate->prefixCommands += type;
    myPrivate->prefixCommands += "\n";
    goString backup2 = myPrivate->shellPostfix;
    myPrivate->shellPostfix = " > \"";
    myPrivate->shellPostfix += filename;
    myPrivate->shellPostfix += "\"";
    bool ok = this->plot();
    myPrivate->prefixCommands = backup1;
    myPrivate->shellPostfix = backup2;
    return ok;
}


