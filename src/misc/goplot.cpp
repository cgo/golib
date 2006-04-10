#include <goplot.h>
#include <goprocess.h>
#include <gofileio.h>
#include <gofixedarray.h>

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
    file = goFileIO::createTempFile(cmd_filename);
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

template < class arrayT >
bool goPlot::gnuplot(const arrayT& a, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor)
{
    return _gnuplot< arrayT > (a, cmdFileNameRet, dataFileNameRet, title, waitfor, plotCommands, prefixCommands, shellPostfix);
}

#define GOPLOT_INSTANTIATE(TYPE) \
template bool goPlot::gnuplot< TYPE > (const TYPE& a, const char* title, const char* plotCommands, const char* prefixCommands, const char* shellPostfix, goString* cmdFileNameRet, goString* dataFileNameRet, bool waitfor);

GOPLOT_INSTANTIATE(goArray<goFloat>);
GOPLOT_INSTANTIATE(goArray<goDouble>);
GOPLOT_INSTANTIATE(goFixedArray<goFloat>);
GOPLOT_INSTANTIATE(goFixedArray<goDouble>);

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
