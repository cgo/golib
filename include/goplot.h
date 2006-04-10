#ifndef GOPLOT_H
#define GOPLOT_H
#include <goarray.h>
#include <gofixedarray.h>
#include <gostring.h>

namespace goPlot
{
    /** 
     * @brief Plots 1D data using gnuplot.
     *
     * Before calling the plot command, <prefixCommands> are executed
     * in gnuplot and then plot <filename> <gnuplotCommands>.
     * If a shell postfix is given, gnuplot is called in
     * a shell like
     * gnuplot <shellPostfix>. You can e.g. use this
     * to redirect output for postscript terminal type.
     * The calling process can either wait for gnuplot to finish
     * or call it the non-blocking way; this is selected with
     * the waitFor parameter.<br>
     * This function is currently implemented for
     * goArray and goFixedArray template classes with
     * goFloat and goDouble template parameters.
     * 
     * @param array Array, currently goArray<> and goFixedArray<>
     *              of types goFloat and goDouble are supported.
     * @param title Title of the plot.
     * @param gnuplotCommands  Commands that come with the plot command,
     *                         like this:
     *                         plot <filename> <gnuplotCommands>. 
     *                         You could for example put "with dots" 
     *                         or similar here. If left null,
     *                         "with lines" is assumed.
     * @param prefixCommands   Gnuplot commands that go before
     *                         the plot command. You can for example
     *                         put commands to select the terminal type
     *                         here.
     * @param shellPostfix     If this is not null,
     *                         gnuplot is called in a shell (/bin/sh)
     *                         and this string is put after the gnuplot
     *                         command string. Use this e.g.
     *                         for "> my_file.ps" if you use
     *                         postscript terminal.
     * @param cmdFileNameRet   If not null, the command file name
     *                         used by gnuplot (a temporary file)
     *                         is stored here after the call.
     * @param dataFileNameRet  If not null, the data file name
     *                         used by gnuplot (a temporary file)
     *                         is stored here after the call.
     * @param waitfor          If true, the calling process blocks
     *                         until gnuplot has finished.
     *                         If false, the calling process does not 
     *                         block.
     * @return                 True if successful, false otherwise.
     */
    template <class arrayT>
        bool gnuplot (const arrayT& array, 
                      const char* title = 0, 
                      const char* gnuplotCommands = 0,
                      const char* prefixCommands = 0,
                      const char* shellPostfix = 0,
                      goString* cmdFileNameRet = 0, 
                      goString* dataFileNameRet = 0, 
                      bool waitfor = false);
#if 0
        static bool gnuplot (const goArray<goFloat>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goArray<goDouble>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goFixedArray<goFloat>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
        static bool gnuplot (const goFixedArray<goDouble>&, const char* title = 0, const char* gnuplotCommands = 0, goString* cmdFileNameRet = 0, goString* dataFileNameRet = 0, bool waitfor = false);
#endif
}

#endif
