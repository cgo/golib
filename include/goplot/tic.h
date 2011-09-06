/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
