#ifndef GOPLOT_H
#define GOPLOT_H
#include <goarray.h>
#include <gostring.h>

class goPlot
{
    public:
        static bool gnuplot (const goArray<goFloat>&, goString& cmdFileNameRet, goString& dataFileNameRet, const char* title = 0);
        static bool gnuplot (const goArray<goDouble>&, goString& cmdFileNameRet, goString& dataFileNameRet, const char* title = 0);
};

#endif
