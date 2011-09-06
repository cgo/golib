/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/*
 * This file and the programs contained in it and in associated files
 * are copyright 2003 by Christian Gosch.
 * Email: christian@goschs.de
 * If no other license is supplied with this file, 
 * assume it is distributable under the GNU General Public License (GPL).
 * $Id$
 */

#include <gotypes.h>
#include <gouniformquantizer.h>
#include <iostream>
#include <stdlib.h>

#include <goplot.h>

int main (int argc, char* argv[])
{
//    if (argc < 2)
//    {
//        std::cout << argv[0] << " <number>\n";
//        exit (1);
//    }

    goUniformQuantizer<goFloat, goInt8> Q (4.0f, -100.0f, 100.0f, -128, 127);
    goUniformQuantizer<goFloat, goFloat> Q2 (1.0f, -100.0f, 100.0f, -1.0, 1.0);

    // goInt8 q = Q.quantize (atof(argv[1]));
    // std::cout << "Q(" << argv[1] << ")" << " = " << (int)q << "\n";
   
    goMatrixf points (201, 2);

    goFloat f;
    int i = 0;
    for (f = -100.0f; f < 101.0f; f += 1.0f, ++i)
    {
        std::cout << f << " " << (float)Q.quantize(f) << "\n";
        points (i, 0) = f;
        points (i, 1) = Q.quantize (f);
    }

    goPlot::plot (points, "");
    
    exit (1);
}

