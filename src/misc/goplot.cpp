#include <goplot.h>
#include <goprocess.h>
#include <gofileio.h>

template<class T>
static bool _gnuplot (const goArray<T>& a, goString& cmdFilenameRet, goString& dataFileNameRet, const char* title)
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
    gnuplotCommands = "plot \"";
    gnuplotCommands += filename.toCharPtr();
    gnuplotCommands += "\" title \"";
    gnuplotCommands += filename.toCharPtr();
    gnuplotCommands += ": "; gnuplotCommands += title ? title : "untitled";
    gnuplotCommands += "\" with lines \n";
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
    gnuplot.run ("gnuplot", cmd_filename.toCharPtr());
    // goFileIO::remove (cmd_filename);
    cmdFilenameRet = cmd_filename;
    dataFileNameRet = filename;
    return true;
}

bool goPlot::gnuplot (const goArray<goFloat>& a, goString& cmdFileNameRet, goString& dataFileNameRet, const char* title)
{
    return _gnuplot<goFloat> (a, cmdFileNameRet, dataFileNameRet, title);
}

bool goPlot::gnuplot (const goArray<goDouble>& a, goString& cmdFileNameRet, goString& dataFileNameRet, const char* title)
{
    return _gnuplot<goDouble> (a, cmdFileNameRet, dataFileNameRet, title);
}
