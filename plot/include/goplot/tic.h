#ifndef GOPLOT_TIC_H
#define GOPLOT_TIC_H

//= Rethink this --- maybe a tic class is not the best idea. There may be many tics.

#include <goplot/plot.h>

namespace goPlot
{
    class Tic : public Object2D
    {
        public:
            Tic ()
                : Object2D ()
            {

            }

            virtual ~Tic ()
            {
            }
    };
};

#endif
