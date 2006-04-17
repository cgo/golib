#include <goplot.h>
#include <goprocess.h>
#include <gofileio.h>
#include <gofixedarray.h>
#include <gomath.h>

static bool _gnuplot_do_plot (const goString& filename, goString* cmdFilenameRet, goString* dataFileNameRet, const char* title, bool waitfor, const char* plotCommands, const char* prefixCommands, const char* shellPostfix)
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

#define GOPLOT_INSTANTIATE(TYPE) \
template bool goPlot::gnuplot< TYPE > (const TYPE& a, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor);

#define GOPLOT_INSTANTIATE2(TYPE,TYPE2) \
template bool goPlot::gnuplot< TYPE , TYPE2 > (const TYPE& a, const TYPE2& a2, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor);

GOPLOT_INSTANTIATE(goArray<goFloat>);
GOPLOT_INSTANTIATE(goArray<goDouble>);
GOPLOT_INSTANTIATE(goFixedArray<goFloat>);
GOPLOT_INSTANTIATE(goFixedArray<goDouble>);
GOPLOT_INSTANTIATE2(goArray<goFloat>,goArray<goFloat>);
GOPLOT_INSTANTIATE2(goArray<goDouble>,goArray<goDouble>);
GOPLOT_INSTANTIATE2(goFixedArray<goFloat>,goFixedArray<goFloat>);
GOPLOT_INSTANTIATE2(goFixedArray<goDouble>,goFixedArray<goDouble>);

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
