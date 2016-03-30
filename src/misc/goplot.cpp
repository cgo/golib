/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot.h>
#include <goprocess.h>
#include <gofileio.h>
#include <gofixedarray.h>
#include <gomath.h>
#include <golist.h>
#ifndef GOLOG_H
# include <golog.h>
#endif
#include <gosignal3dbase.h>
#include <gosignal3dgenericiterator.h>

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

static bool _gnuplot_do_plot (goString filename, goString* cmdFilenameRet, goString* dataFileNameRet, const char* title, bool waitfor, const char* plotCommands, const char* prefixCommands, const char* shellPostfix)
{
    goString gnuplotCommands;
    if (prefixCommands)
    {
        gnuplotCommands += prefixCommands;
        gnuplotCommands += "\n";
    }
    gnuplotCommands += "plot \"";
    gnuplotCommands += filename.toCharPtr();
    gnuplotCommands += "\" title \"";
    // gnuplotCommands += filename.toCharPtr();
    // gnuplotCommands += ": "; 
    gnuplotCommands += title ? title : "untitled";
    gnuplotCommands += "\"";
    if (plotCommands)
    {
        gnuplotCommands += " ";
        gnuplotCommands += plotCommands;
        gnuplotCommands += "\n";
    }   
    else
    {
        gnuplotCommands += " with lines \n";
    }
    gnuplotCommands += "pause -1\n";
    goString cmd_filename;
    FILE* file = goFileIO::createTempFile(cmd_filename);
    if (!file)
    {
        return false;
    }
    fprintf (file, "%s", gnuplotCommands.toCharPtr());
    fclose (file);
    goProcess gnuplot;
    if (shellPostfix)
    {
        goFixedArray<goString> cmds(2);
        goString shellCommand = "/bin/sh";
        // cmds += &shellCommand;
        cmds[0] = "-c";
        goString gpCommand = "gnuplot ";
        gpCommand += cmd_filename.toCharPtr();
        gpCommand += shellPostfix;
        cmds[1] = gpCommand;
        gnuplot.run (shellCommand.toCharPtr(), cmds);
    }
    else
    {
        gnuplot.run ("gnuplot", cmd_filename.toCharPtr());
    }
    if (cmdFilenameRet)
        *cmdFilenameRet = cmd_filename;
    if (dataFileNameRet)
        *dataFileNameRet = filename;
    if (waitfor)
    {
        gnuplot.wait();
    }
    // goFileIO::remove (cmd_filename);
    return true;
}


static bool _gnuplot_do_plot_list (goString* cmdFilenameRet, 
                                   const goList<goString>* dataFileNames, 
                                   const goList<goString>* titles, 
                                   bool waitfor, 
                                   const goList<goString>* plotCommands, 
                                   const char* prefixCommands, 
                                   const char* postfixCommands, 
                                   const char* shellPostfix)
{
#if 0
    if (!dataFileNames)
    {
        return false;
    }
    if (titles)
    {
        if (dataFileNames->getSize() != titles->getSize())
        {
            goLog::warning ("goPlot::gnuplot(): (list version) dataFileNameRet and titles are of different sizes.");
            return false;
        }
    }
    if (plotCommands)
    {
        if (dataFileNames->getSize() != plotCommands->getSize())
        {
            goLog::warning ("goPlot::gnuplot(): (list version) dataFileNameRet and plotCommands are of different sizes.");
            return false;
        }
    }

    goList<goString>::ConstElement* dataFileNameEl = dataFileNames->getFrontElement();
    goList<goString>::ConstElement* titlesEl       = 0;
    if (titles)
    {
        titlesEl = titles->getFrontElement();
    }
    goList<goString>::ConstElement* plotCommandsEl = 0;
    if (plotCommands)
    {
        plotCommandsEl = plotCommands->getFrontElement();
    }
    
    goString gnuplotCommands;
    if (prefixCommands)
    {
        gnuplotCommands += prefixCommands;
        gnuplotCommands += "\n";
    }
    gnuplotCommands += "plot ";
    while (dataFileNameEl)
    {
        gnuplotCommands += "\"";
        gnuplotCommands += dataFileNameEl->elem.toCharPtr();
        gnuplotCommands += "\"";
        if (!titlesEl)
        {
            gnuplotCommands += "notitle ";
        }
        else
        {
            gnuplotCommands += " title \"";
            gnuplotCommands += titlesEl->elem.toCharPtr();
            gnuplotCommands += "\"";
        }
        if (plotCommandsEl)
        {
            gnuplotCommands += " ";
            gnuplotCommands += plotCommandsEl->elem.toCharPtr();
        }   
        else
        {
            gnuplotCommands += " with lines";
        }
        if (dataFileNameEl->next)
        {
            gnuplotCommands += ", ";
        }
        else 
        {
            gnuplotCommands += "\n";
        }
        dataFileNameEl = dataFileNameEl->next;
        if (titlesEl)       titlesEl = titlesEl->next;
        if (plotCommandsEl) plotCommandsEl = plotCommandsEl->next;
    }
    // gnuplotCommands += "pause -1\n";
    if (postfixCommands)
    {
        gnuplotCommands += postfixCommands;
    }
    else
    {
        gnuplotCommands += "\n";
    }
#endif
    goString gnuplotCommands = "";
    if (!goPlot::addGnuplotCommands (gnuplotCommands,
                                     dataFileNames,
                                     titles,
                                     plotCommands,
                                     prefixCommands,
                                     postfixCommands))
    {
        return false;
    }
    return goPlot::callGnuplot (gnuplotCommands, 
                                shellPostfix, 
                                waitfor, 
                                cmdFilenameRet);
}

template<class arrayT>
static bool _gnuplot (const arrayT& a, goString* cmdFilenameRet, goString* dataFileNameRet, const char* title, bool waitfor, const char* plotCommands, const char* prefixCommands, const char* shellPostfix)
{
    goString filename;
    FILE* file = goFileIO::createTempFile (filename);
    if (!file)
        return false;
    goIndex_t i;
    goIndex_t size = a.getSize();
    for (i = 0; i < size; ++i)
    {
        fprintf (file, "%f\n", a[i]);
    }
    fclose (file);
    
    return _gnuplot_do_plot (filename, cmdFilenameRet, dataFileNameRet, title, waitfor, plotCommands, prefixCommands, shellPostfix);
}


template<class arrayT>
static bool _gnuplot_list (const goList<arrayT>* arrayListX, 
                           const goList<arrayT>* arrayListY, 
                           goString* cmdFilenameRet, 
                           goList<goString>* dataFileNameRet, 
                           goList<goString>* titles, 
                           bool waitfor, 
                           goList<goString>* plotCommands, 
                           const char* prefixCommands, 
                           const char* postfixCommands, 
                           const char* shellPostfix)
{
#if 0
    if (!arrayListX)
    {
        return false;
    }
    if (arrayListY)
    {
        if (arrayListX->getSize() != arrayListY->getSize())
        {
            goLog::warning("goPlot::gnuplot(): (list version) arrayListX must be of same size as arrayListY.");
            return false;
        }
    }
    if (!waitfor && !dataFileNameRet)
    {
        goLog::warning("goPlot::gnuplot(): (list version) if waitfor is false, dataFileNameRet must be set (or you will get segfaults.)");
        return false;
    }
    goList<goString> filenames_;
    goList<goString>* filenames = &filenames_;
    if (dataFileNameRet)
    {
        filenames = dataFileNameRet;
    }
    filenames->erase(); 
    typename goList<arrayT>::ConstElement* elX = arrayListX->getFrontElement();
    typename goList<arrayT>::ConstElement* elY = 0;
    if (arrayListY)
    {
        elY = arrayListY->getFrontElement();
        assert (arrayListX->getSize() == arrayListY->getSize());
    }
    while (elX)
    {
        goString filename;
        FILE* file = goFileIO::createTempFile (filename);
        if (!file)
            return false;
        filenames->append(filename);
        goIndex_t i;
        goIndex_t size = elX->elem.getSize();
        if (arrayListY)
        {
            assert (size == static_cast<goIndex_t>(elY->elem.getSize()));
            for (i = 0; i < size; ++i)
            {
                fprintf (file, "%f %f\n", elX->elem[i], elY->elem[i]);
            }
        }
        else
        {
            for (i = 0; i < size; ++i)
            {
                fprintf (file, "%f\n", elX->elem[i]);
            }
        }
        elX = elX->next;
        if (elY) elY = elY->next;
        fclose (file);
    }
#endif
    goList<goString> filenames_;
    goList<goString>* filenames = &filenames_;
    if (dataFileNameRet)
    {
        filenames = dataFileNameRet;
    }
    if (!goPlot::writeGnuplotDataFiles (arrayListX, arrayListY, *filenames))
    {
        return false;
    }
    return _gnuplot_do_plot_list (cmdFilenameRet, filenames, titles, waitfor, plotCommands, prefixCommands, postfixCommands, shellPostfix);
}

template<class arrayT, class arrayT2>
static bool _gnuplot (const arrayT& a, const arrayT2& a2, goString* cmdFilenameRet, goString* dataFileNameRet, const char* title, bool waitfor, const char* plotCommands, const char* prefixCommands, const char* shellPostfix)
{
    goString filename;
    FILE* file = goFileIO::createTempFile (filename);
    if (!file)
        return false;
    goIndex_t i;
    goIndex_t size = goMath::min<goIndex_t>(a.getSize(),a2.getSize());
    for (i = 0; i < size; ++i)
    {
        fprintf (file, "%f %f\n", a[i], a2[i]);
    }
    fclose (file);
    
    return _gnuplot_do_plot (filename, cmdFilenameRet, dataFileNameRet, title, waitfor, plotCommands, prefixCommands, shellPostfix);
}

bool goPlot::addGnuplotCommands 
              (goString& gnuplotCommands,
               const goList<goString>* dataFileNames, 
               const goList<goString>* titles, 
               const goList<goString>* plotCommands, 
               const char* prefixCommands,
               const char* postfixCommands,
               goPlot::PlotType plotType)
{
    if (!dataFileNames)
    {
        return false;
    }
    if (dataFileNames && dataFileNames->getSize() == 0)
    {
        return true;
    }
    if (titles)
    {
        if (dataFileNames->getSize() != titles->getSize())
        {
            goString s = "goPlot::addGnuplotCommands(): (list version) dataFileNames and titles are of different sizes (";
            s += (int)dataFileNames->getSize();
            s += " != ";
            s += (int)titles->getSize();
            s += ")";
            goLog::warning (s.toCharPtr());
            return false;
        }
    }
    if (plotCommands && dataFileNames)
    {
        if (dataFileNames->getSize() != plotCommands->getSize())
        {
            goString s = "goPlot::addGnuplotCommands(): (list version) dataFileNames and plotCommands are of different sizes (";
            s += (int)dataFileNames->getSize();
            s += " != ";
            s += (int)plotCommands->getSize();
            s += ")";
            goLog::warning (s.toCharPtr());
            return false;
        }
    }

    goList<goString>::ConstElement* dataFileNameEl = 0;
    if (dataFileNames)
    {
        dataFileNameEl = dataFileNames->getFrontElement();
    }
    goList<goString>::ConstElement* titlesEl       = 0;
    if (titles)
    {
        titlesEl = titles->getFrontElement();
    }
    goList<goString>::ConstElement* plotCommandsEl = 0;
    if (plotCommands)
    {
        plotCommandsEl = plotCommands->getFrontElement();
    }
    
    if (prefixCommands)
    {
        gnuplotCommands += prefixCommands;
        gnuplotCommands += "\n";
    }
    switch (plotType)
    {
        case goPlot::Normal: gnuplotCommands += "plot "; break;
        case goPlot::Surface: gnuplotCommands += "splot "; break;
    }
    while (dataFileNameEl)
    {
        gnuplotCommands += "\"";
        gnuplotCommands += dataFileNameEl->elem.toCharPtr();
        gnuplotCommands += "\"";
        if (titlesEl)
        {
            gnuplotCommands += " title \"";
            gnuplotCommands += titlesEl->elem.toCharPtr();
            gnuplotCommands += "\"";
        }
        if (plotCommandsEl)
        {
            gnuplotCommands += " ";
            gnuplotCommands += plotCommandsEl->elem.toCharPtr();
        }   
        else
        {
            gnuplotCommands += " with lines";
        }
        if (dataFileNameEl->next)
        {
            gnuplotCommands += ", ";
        }
        else 
        {
            gnuplotCommands += "\n";
        }
        dataFileNameEl = dataFileNameEl->next;
        if (titlesEl)       titlesEl = titlesEl->next;
        if (plotCommandsEl) plotCommandsEl = plotCommandsEl->next;
    }
    if (postfixCommands)
    {
        gnuplotCommands += postfixCommands;
    }
    else
    {
        gnuplotCommands += "\n";
    }
    switch (plotType)
    {
        case goPlot::Normal: break; 
        case goPlot::Surface: break; // gnuplotCommands += "unset pm3d\n"; break;
    }
    return true;
}

bool goPlot::callGnuplot (const goString& gnuplotCommands, 
                          const char*     shellPostfix, 
                          bool            waitfor, 
                          goString*       cmdFilenameRet,
                          int             redirectInputFD,
                          int             redirectOutputFD)
{
    goString cmd_filename;
    FILE* file = goFileIO::createTempFile(cmd_filename);
    if (!file)
    {
        return false;
    }
    fprintf (file, "%s", gnuplotCommands.toCharPtr());
    fclose (file);
    goProcess gnuplot;
    gnuplot.setInput  (redirectInputFD);
    gnuplot.setOutput (redirectOutputFD);
    if (shellPostfix)
    {
        goFixedArray<goString> cmds(2);
        goString shellCommand = "/bin/sh";
        // cmds += &shellCommand;
        cmds[0] = "-c";
        goString gpCommand = "gnuplot ";
        gpCommand += cmd_filename.toCharPtr();
        gpCommand += shellPostfix;
        cmds[1] = gpCommand;
        gnuplot.run (shellCommand.toCharPtr(), cmds);
    }
    else
    {
        gnuplot.run ("gnuplot", cmd_filename.toCharPtr());
    }
    if (cmdFilenameRet)
        *cmdFilenameRet = cmd_filename;
    if (waitfor)
    {
        gnuplot.wait();
    }
    // goFileIO::remove (cmd_filename);
    return true;
}

template <class arrayT>
bool goPlot::writeGnuplotDataFiles (const goList<arrayT>* arrayListX, 
                                    const goList<arrayT>* arrayListY, 
                                    goList<goString>& dataFileNameRet)
{
    if (!arrayListX)
    {
        return false;
    }
    if (arrayListY)
    {
        if (arrayListX->getSize() != arrayListY->getSize())
        {
            goLog::warning("goPlot::writeGnuplotDataFiles(): (list version) arrayListX must be of same size as arrayListY.");
            return false;
        }
    }
    goList<goString>* filenames = &dataFileNameRet;

    // filenames->erase(); 
    typename goList<arrayT>::ConstElement* elX = arrayListX->getFrontElement();
    typename goList<arrayT>::ConstElement* elY = 0;
    if (arrayListY)
    {
        elY = arrayListY->getFrontElement();
        assert (arrayListX->getSize() == arrayListY->getSize());
    }
    while (elX)
    {
        goString filename;
        FILE* file = goFileIO::createTempFile (filename);
        if (!file)
            return false;
        filenames->append(filename);
        goIndex_t i;
        goIndex_t size = elX->elem.getSize();
        if (arrayListY)
        {
            assert (size == static_cast<goIndex_t>(elY->elem.getSize()));
            for (i = 0; i < size; ++i)
            {
                fprintf (file, "%f %f\n", elX->elem[i], elY->elem[i]);
            }
        }
        else
        {
            for (i = 0; i < size; ++i)
            {
                fprintf (file, "%f\n", elX->elem[i]);
            }
        }
        elX = elX->next;
        if (elY) elY = elY->next;
        fclose (file);
    }
    return true;
}

template <class arrayT>
bool goPlot::writeGnuplotDataFiles (const goList<arrayT>* arrayListX, 
                                    const goList<arrayT>* arrayListY, 
                                    const goList<arrayT>* arrayListZ,
                                    goIndex_t             lineLength,
                                    goList<goString>& dataFileNameRet)
{
    if (!arrayListX || !arrayListY || !arrayListZ)
    {
        return false;
    }
    if (arrayListX->getSize() != arrayListY->getSize())
    {
        goLog::warning("goPlot::writeGnuplotDataFiles(): (list version) arrayListX must be of same size as arrayListY.");
        return false;
    }
    if (arrayListZ->getSize() != arrayListY->getSize())
    {
        goLog::warning("goPlot::writeGnuplotDataFiles(): (list version) arrayListZ must be of same size as arrayListY.");
        return false;
    }
    goList<goString>* filenames = &dataFileNameRet;

    // filenames->erase(); 
    typename goList<arrayT>::ConstElement* elX = arrayListX->getFrontElement();
    typename goList<arrayT>::ConstElement* elY = arrayListY->getFrontElement();
    typename goList<arrayT>::ConstElement* elZ = arrayListZ->getFrontElement();
    assert (arrayListX->getSize() == arrayListY->getSize());
    goIndex_t lineCounter = 0;
    while (elX)
    {
        goString filename;
        FILE* file = goFileIO::createTempFile (filename);
        if (!file)
            return false;
        filenames->append(filename);
        goIndex_t i;
        goIndex_t size = elX->elem.getSize();
        assert (size == static_cast<goIndex_t>(elY->elem.getSize()));
        assert (size == static_cast<goIndex_t>(elZ->elem.getSize()));
        for (i = 0; i < size; ++i)
        {
            fprintf (file, "%f %f %f\n", elX->elem[i], elY->elem[i], elZ->elem[i]);
            ++lineCounter;
            if (lineCounter >= lineLength)
            {
                lineCounter = 0;
                fprintf (file, "\n");
            }
        }
        elX = elX->next;
        elY = elY->next;
        elZ = elZ->next;
        fclose (file);
    }
    return true;
}

template <class T>
bool goPlot::writeGnuplotDataFiles (const goList<goMath::Matrix<T> >*    matrices,
                                    goList<goString>&     dataFileNameRet)
{
    goList<goString>* filenames = &dataFileNameRet;

    // filenames->erase(); 
    goString filename;

    typename goList<goMath::Matrix<T> >::ConstElement* el = matrices->getFrontElement ();
    while (el)
    {
        FILE* file = goFileIO::createTempFile (filename);
        if (!file)
            return false;
        filenames->append(filename);
        const goMath::Matrix<T>& matrix = el->elem;
        for (goSize_t row = 0; row < matrix.getRows(); ++row)
        {
            for (goSize_t col = 0; col < matrix.getColumns(); ++col)
            {
                fprintf (file, "%f %f %f\n", (float)row, (float)col, (float)matrix(row,col));
            }
            fprintf (file, "\n");
        }
        fclose (file);
        el = el->next;
    }
    return true;
}

template <class T>
static bool writeGnuplotDataFilesSignal3D (
        const goSignal3DBase<void>* image,
        FILE* file)
{
    goSignal3DGenericConstIterator it (image);
    goSize_t y = 0;
    while (!it.endY())
    {
        it.resetX();
        goSize_t x = 0;
        while (!it.endX())
        {
            fprintf (file, "%f %f %f\n", (float)x, (float)y, (float)*(T*)*it);
            it.incrementX();
            ++x;
        }
        fprintf (file, "\n");
        it.incrementY();
        ++y;
    }
    return true;
}

template <class T>
bool goPlot::writeGnuplotDataFilesBinary (const goList<goMath::Matrix<T> >* images,
                                          goList<goString>&        dataFileNameRet)
{
    goString filename;
    typename goList<goMath::Matrix<T> >::ConstElement *el = images->getFrontElement();
    while (el)
    {
        FILE* f = goFileIO::createTempFile (filename);
        if (!f)
            return false;
        goFileIO::writeBinaryMatrix (el->elem, f);
        dataFileNameRet.append (filename);
        fclose (f);
        el = el->next;
    }
    return true;    
}

bool goPlot::writeGnuplotDataFiles (const goList<const goSignal3DBase<void>*>* images,
                                    goList<goString>&     dataFileNameRet)
{
    if (!images)
        return false;

    goList<goString>* filenames = &dataFileNameRet;

    // filenames->erase(); 
    goString filename;

    goList<const goSignal3DBase<void>* >::ConstElement* el = images->getFrontElement ();
    while (el)
    {
        if (el->elem)
        {
            FILE* file = goFileIO::createTempFile (filename);
            if (!file)
            {
                goString msg = "goPlot::writeGnuplotDataFiles() for goSignal3D: can not open file ";
                msg += filename.toCharPtr();
                msg += " for writing.";
                el = el->next;
                continue;
            }
            filenames->append(filename);
            switch (el->elem->getDataType().getID())
            {
                case GO_UINT8:  writeGnuplotDataFilesSignal3D<goUInt8>  (el->elem, file); break;
                case GO_INT8:   writeGnuplotDataFilesSignal3D<goInt8>   (el->elem, file); break;
                case GO_UINT16: writeGnuplotDataFilesSignal3D<goUInt16> (el->elem, file); break;
                case GO_INT16:  writeGnuplotDataFilesSignal3D<goInt16>  (el->elem, file); break;
                case GO_UINT32: writeGnuplotDataFilesSignal3D<goUInt32> (el->elem, file); break;
                case GO_INT32:  writeGnuplotDataFilesSignal3D<goInt32>  (el->elem, file); break;
                case GO_FLOAT:  writeGnuplotDataFilesSignal3D<goFloat>  (el->elem, file); break;
                case GO_DOUBLE: writeGnuplotDataFilesSignal3D<goDouble> (el->elem, file); break;
                default: break;
            }
            fclose (file);
        }
        el = el->next;
    }
    return true;
}

/** 
 * @brief Convenience function for quickly plotting something.
 *
 * For quick one-line plotting commands.
 * Creates a goMultiPlotter and goSinglePlot, adds the given curve with options and
 * prefix commands, sets the pause flag in goMultiPlotter to true (so that
 * each plot waits for a key press) and calls plot().
 * 
 * @param x X coordinates.
 * @param y Y coordinates.
 * @param title Title (of the curve).
 * @param plotOptions Options for gnuplot (like "with lines")
 * @param prefix Prefix commands for gnuplot (like "set ..." commands)
 */
template <class T>
void goPlot::plot (const goMath::Vector<T>& x, const goMath::Vector<T>& y, 
                const char* title, const char* plotOptions, const char* prefix)
{
    goMultiPlotter plotter (1,1);
    goSinglePlot plot;
    plot.addCurve (x,y,title ? title : "", plotOptions);
    if (prefix)
        plot.setPrefix (prefix);
    plotter.addPlot (plot,0,0);
    plotter.setPauseFlag (true);
    plotter.plot ();
}

template <class T>
void goPlot::plot (const goMath::Vector<T>& y, 
                const char* title, const char* plotOptions, const char* prefix)
{
    goMultiPlotter plotter (1,1);
    goSinglePlot plot;
    plot.addCurve (y,title ? title : "", plotOptions);
    if (prefix)
        plot.setPrefix (prefix);
    plotter.addPlot (plot,0,0);
    plotter.setPauseFlag (true);
    plotter.plot ();
}

/** 
 * @brief Convenience function for quickly plotting something.
 *
 * For quick one-line plotting commands.
 * Creates a goMultiPlotter and goSinglePlot, adds the given curve with options and
 * prefix commands, sets the pause flag in goMultiPlotter to true (so that
 * each plot waits for a key press) and calls plot().
 * 
 * @param points N x 2 point configuration matrix of the curve.
 * @param title Title (of the curve).
 * @param plotOptions Options for gnuplot (like "with lines")
 * @param prefix Prefix commands for gnuplot (like "set ..." commands)
 */
template <class T>
void goPlot::plot (const goMath::Matrix<T>& points,
                const char* title, const char* plotOptions, const char* prefix)
{
    goMultiPlotter plotter (1,1);
    goSinglePlot plot;
    if (prefix)
        plot.setPrefix (prefix);
    plot.addCurveMatrix (points,title ? title : "", plotOptions);
    plotter.addPlot (plot,0,0);
    plotter.setPauseFlag (true);
    plotter.plot ();
}

template <class T>
void goPlot::plot3D (const goMath::Matrix<T>& M,
        const char* title, const char* plotOptions, const char* prefix, bool separateRows)
{
    goMultiPlotter plotter (1,1);
    goSinglePlot plot;
    if (prefix)
        plot.setPrefix (prefix);
    plot.add3D (M, title, plotOptions, separateRows);
    plotter.addPlot (plot,0);
    plotter.setPauseFlag (true);
    plotter.plot ();
}

template < class arrayT >
bool goPlot::gnuplot(const arrayT& a, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor)
{
    return _gnuplot< arrayT > (a, cmdFileNameRet, dataFileNameRet, title, waitfor, plotCommands, prefixCommands, shellPostfix);
}

template < class arrayT , class arrayT2 >
bool goPlot::gnuplot(const arrayT& a, const arrayT2& a2, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor)
{
    return _gnuplot< arrayT, arrayT2> (a, a2, cmdFileNameRet, dataFileNameRet, title, waitfor, plotCommands, prefixCommands, shellPostfix);
}

template <class arrayT>
bool goPlot::gnuplotList (const goList< arrayT >* arrayListX,
                          const goList< arrayT >* arrayListY,
                          goList<goString>*     title, 
                          goList<goString>* plotCommands,
                          const char*       prefixCommands,
                          const char*       postfixCommands,
                          const char*       shellPostfix,
                          goString*         cmdFileNameRet, 
                          goList<goString>* dataFileNameRet, 
                          bool waitfor)
{
    return _gnuplot_list <arrayT> (arrayListX, arrayListY, cmdFileNameRet, dataFileNameRet, title, waitfor, plotCommands, prefixCommands, postfixCommands, shellPostfix);
}


//= Support functions for the goPlot library (not gnuplot)

//= In the header file.
//template <class T>
//goPlot::AutoPtr<goPlot::Object2DPoints<Points2DMatrix<T>,T > > goPlot::object2D (const goMatrix<T>& curve)
//{
//    AutoPtr<Object2DPoints<Points2DMatrix<T>,T> > points = new Object2DPoints<Points2DMatrix<T>,T>;
//    assert (!points.isNull ());
//    points->points() = curve;
//
//    return points;
//}

/** 
 * @brief Create a goAutoPtr<goPlot::Object2DImage> for use with the goPlot library.
 * 
 * The given 2D image \c img is copied into a \c goPlot::Object2DImage.
 * If \c img has 3 or 4 channels, they are interpreted as RGB / RGBA
 * and converted to ARGB32 for display.
 * 1-channel images are converted to A8 format, which is only an alpha.
 * It may be a good idea to change this.
 *
 * @todo Convert 1-channel images also to ARGB32
 *
 * @param img The 2D source image.
 * 
 * @return The goAutoPtr to the image object.
 */
goAutoPtr<goPlot::Object2DImage> goPlot::object2DImage (const goSignal3DBase<void>& img)
{
    int format = goPlot::Object2DImage::ARGB32;

    switch (img.getChannelCount ())
    {
        case 4:
        case 3: format = goPlot::Object2DImage::ARGB32;
                printf ("Format: ARGB32\n");
                break;
        case 1: format = goPlot::Object2DImage::A8;
                printf ("Format: A8\n");
                break;
        default: goLog::error ("goPlot::object2DImage(): unknown channel count.");
                 break;
    }

    goAutoPtr<goPlot::Object2DImage> ret = new goPlot::Object2DImage;

    ret->createImage (format, img.getSizeX(), img.getSizeY());

    int strides[] = {1, ret->stride(), ret->stride() * ret->height()}; 
    goCopySignalArray<unsigned char> (&img, ret->data (), strides);

    return ret;
}

#define GOPLOT_INSTANTIATE(TYPE) \
template bool goPlot::gnuplot< TYPE > (const TYPE& a, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor);

#define GOPLOT_INSTANTIATE2(TYPE,TYPE2) \
template bool goPlot::gnuplot< TYPE , TYPE2 > (const TYPE& a, const TYPE2& a2, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor);

#define GOPLOT_INSTANTIATE_LIST(TYPE) \
template bool goPlot::gnuplotList< TYPE > (const goList< TYPE >* arrayListX,\
              const goList< TYPE >* arrayListY,\
              goList<goString>*     title,\
              goList<goString>* plotCommands,\
              const char*       prefixCommands,\
              const char*       postfixCommands,\
              const char*       shellPostfix,\
              goString*         cmdFileNameRet,\
              goList<goString>* dataFileNameRet,\
              bool waitfor);

#define GOPLOT_WRITEFILES_INSTANTIATE(TYPE) \
template bool goPlot::writeGnuplotDataFiles<TYPE> (const goList<TYPE>* arrayListX, \
                                    const goList<TYPE>* arrayListY, \
                                    goList<goString>& dataFileNameRet);

#define GOPLOT_WRITEFILES_INSTANTIATE2(TYPE) \
template bool goPlot::writeGnuplotDataFiles<TYPE> (const goList<TYPE>* arrayListX, \
                                    const goList<TYPE>* arrayListY, \
                                    const goList<TYPE>* arrayListZ, \
                                    goIndex_t lineLength, \
                                    goList<goString>& dataFileNameRet);

#define GOPLOT_WRITEFILES_INSTANTIATE3(TYPE) \
template bool goPlot::writeGnuplotDataFiles<TYPE> (const goList<goMath::Matrix<TYPE> >*    matrix, \
                                    goList<goString>&     dataFileNameRet);

#define GOPLOT_WRITEFILES_BINARY_INSTANTIATE(TYPE) \
template bool goPlot::writeGnuplotDataFilesBinary<TYPE> (const goList<goMath::Matrix<TYPE> >*    matrix, \
                                    goList<goString>&     dataFileNameRet);

GOPLOT_INSTANTIATE(goArray<goFloat>);
GOPLOT_INSTANTIATE(goArray<goDouble>);
GOPLOT_INSTANTIATE(goFixedArray<goFloat>);
GOPLOT_INSTANTIATE(goFixedArray<goDouble>);
GOPLOT_INSTANTIATE2(goArray<goFloat>,goArray<goFloat>);
GOPLOT_INSTANTIATE2(goArray<goDouble>,goArray<goDouble>);
GOPLOT_INSTANTIATE2(goFixedArray<goFloat>,goFixedArray<goFloat>);
GOPLOT_INSTANTIATE2(goFixedArray<goDouble>,goFixedArray<goDouble>);
GOPLOT_INSTANTIATE_LIST(goFixedArray<goFloat>);
GOPLOT_INSTANTIATE_LIST(goFixedArray<goDouble>);
GOPLOT_INSTANTIATE_LIST(goMath::Vectorf);
GOPLOT_INSTANTIATE_LIST(goMath::Vectord);
GOPLOT_WRITEFILES_INSTANTIATE(goArray<goFloat>);
GOPLOT_WRITEFILES_INSTANTIATE(goArray<goDouble>);
GOPLOT_WRITEFILES_INSTANTIATE(goFixedArray<goFloat>);
GOPLOT_WRITEFILES_INSTANTIATE(goFixedArray<goDouble>);
GOPLOT_WRITEFILES_INSTANTIATE2(goArray<goFloat>);
GOPLOT_WRITEFILES_INSTANTIATE2(goArray<goDouble>);
GOPLOT_WRITEFILES_INSTANTIATE2(goFixedArray<goFloat>);
GOPLOT_WRITEFILES_INSTANTIATE2(goFixedArray<goDouble>);
GOPLOT_WRITEFILES_INSTANTIATE(goMath::Vectorf);
GOPLOT_WRITEFILES_INSTANTIATE(goMath::Vectord);
GOPLOT_WRITEFILES_INSTANTIATE2(goMath::Vectorf);
GOPLOT_WRITEFILES_INSTANTIATE2(goMath::Vectord);
GOPLOT_WRITEFILES_INSTANTIATE3(goFloat);
GOPLOT_WRITEFILES_INSTANTIATE3(goDouble);
GOPLOT_WRITEFILES_BINARY_INSTANTIATE(goFloat);
GOPLOT_WRITEFILES_BINARY_INSTANTIATE(goDouble);

template 
void goPlot::plot<goFloat> (
        const goMath::Vector<goFloat>&, 
        const goMath::Vector<goFloat>&, 
        const char*, const char*, const char*);
template 
void goPlot::plot<goDouble> (
        const goMath::Vector<goDouble>&, 
        const goMath::Vector<goDouble>&, 
        const char*, const char*, const char*);
template 
void goPlot::plot<goFloat> (
        const goMath::Vector<goFloat>&, 
        const char*, const char*, const char*);
template 
void goPlot::plot<goDouble> (
        const goMath::Vector<goDouble>&, 
        const char*, const char*, const char*);
template 
void goPlot::plot<goFloat> (
        const goMath::Matrix<goFloat>&, 
        const char*, const char*, const char*);
template 
void goPlot::plot<goDouble> (
        const goMath::Matrix<goDouble>&, 
        const char*, const char*, const char*);
template 
void goPlot::plot3D<goFloat> (
        const goMath::Matrix<goFloat>&, 
        const char*, const char*, const char*, bool);
template 
void goPlot::plot3D<goDouble> (
        const goMath::Matrix<goDouble>&, 
        const char*, const char*, const char*, bool);

#if 0
#define GOPLOT_INSTANTIATE(TYPE) \
bool goPlot::gnuplot (const TYPE& a, const char* title, const char* plotCommands, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor) \
{ \
    return _gnuplot< TYPE > (a, cmdFileNameRet, dataFileNameRet, title, waitfor, plotCommands); \
}

GOPLOT_INSTANTIATE(goArray<goFloat>);
GOPLOT_INSTANTIATE(goArray<goDouble>);
GOPLOT_INSTANTIATE(goFixedArray<goFloat>);
GOPLOT_INSTANTIATE(goFixedArray<goDouble>);

#undef GOPLOT_INSTANTIATE
#endif

#if 0
bool goPlot::gnuplot (const goArray<goFloat>& a, const char* title, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor)
{
    return _gnuplot<goArray<goFloat> > (a, cmdFileNameRet, dataFileNameRet, title, waitfor);
}

bool goPlot::gnuplot (const goArray<goDouble>& a, const char* title, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor)
{
    return _gnuplot<goArray<goDouble> > (a, cmdFileNameRet, dataFileNameRet, title, waitfor);
}

bool goPlot::gnuplot (const goFixedArray<goFloat>& a, const char* title, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor)
{
    return _gnuplot<goFixedArray<goFloat> > (a, cmdFileNameRet, dataFileNameRet, title, waitfor);
}

bool goPlot::gnuplot (const goFixedArray<goDouble>& a, const char* title, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor)
{
    return _gnuplot<goFixedArray<goDouble> > (a, cmdFileNameRet, dataFileNameRet, title, waitfor);
}
#endif

//#ifndef GOLIST_HPP
//#include <golist.hpp>
//template class goList<goPlotterLabel>;
//#endif
