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

int main (int argc, char* argv[])
{
//    if (argc < 2)
//    {
//        std::cout << argv[0] << " <number>\n";
//        exit (1);
//    }

    Go::goUniformQuantizer<goFloat, goInt8> Q (5.0f, -100.0f, 100.0f, -128, 127);

    // goInt8 q = Q.quantize (atof(argv[1]));
    // std::cout << "Q(" << argv[1] << ")" << " = " << (int)q << "\n";
    
    goFloat f;
    for (f = -100.0f; f < 101.0f; f += 1.0f)
    {
        std::cout << f << " " << (int)Q.quantize(f) << "\n";
    }
    
    exit (1);
}

