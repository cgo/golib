#include <goplot.h>
#include <goprocess.h>
#include <gofileio.h>
#include <gofixedarray.h>
#include <gomath.h>
#include <golist.h>
#ifndef GOLOG_H
# include <golog.h>
#endif


class goPlotterPrivate
{
    public:
        goPlotterPrivate() : plotX(), plotY(), titles(), plotCommands(), prefixCommands(""), shellPostfix(""),
                             waitFlag(true), pauseFlag(false), cmdFilename(""), dataFilenames() {};
        ~goPlotterPrivate() {};

        goList<goVectord>      plotX;
        goList<goVectord>      plotY;
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

bool goPlotter::addCurve (const goVectord& x, const goVectord& y, const char* title, const char* plotOptions)
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
bool goPlotter::addCurve (const goVectorf& x, const goVectorf& y, const char* title, const char* plotOptions)
{
    if (x.getSize() != y.getSize())
    {
        goLog::warning("addCurve(): x and y array sizes mismatch.",this);
        return false;
    }
    goVectord xx;
    xx = x;
    goVectord yy;
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
               const char* postfixCommands)
{
    if (!dataFileNames)
    {
        return false;
    }
    if (dataFileNames->getSize() == 0)
    {
        return true;
    }
    if (titles)
    {
        if (dataFileNames->getSize() != titles->getSize())
        {
            goLog::warning ("goPlot::addGnuplotCommands(): (list version) dataFileNameRet and titles are of different sizes.");
            return false;
        }
    }
    if (plotCommands)
    {
        if (dataFileNames->getSize() != plotCommands->getSize())
        {
            goLog::warning ("goPlot::addGnuplotCommands(): (list version) dataFileNameRet and plotCommands are of different sizes.");
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
    if (postfixCommands)
    {
        gnuplotCommands += postfixCommands;
    }
    else
    {
        gnuplotCommands += "\n";
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
    return true;
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
GOPLOT_INSTANTIATE_LIST(goVectorf);
GOPLOT_INSTANTIATE_LIST(goVectord);
GOPLOT_WRITEFILES_INSTANTIATE(goArray<goFloat>);
GOPLOT_WRITEFILES_INSTANTIATE(goArray<goDouble>);
GOPLOT_WRITEFILES_INSTANTIATE(goFixedArray<goFloat>);
GOPLOT_WRITEFILES_INSTANTIATE(goFixedArray<goDouble>);
GOPLOT_WRITEFILES_INSTANTIATE(goVectorf);
GOPLOT_WRITEFILES_INSTANTIATE(goVectord);

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

#ifndef GOLIST_HPP
#include <golist.hpp>
template class goList<goPlotterLabel>;
#endif
