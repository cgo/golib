/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>
#include <goplot.h>
#include <stdio.h>

int main ()
{
    goFloat n0_[] = {0.0, 0.4, 1.0};
    goFloat b0_[] = {0.0, 0.0, 0.5};
    goFloat n1_[] = {1.0, 1.0, 1.0};
    goFloat b1_[] = {0.2, 0.2, 1.0};

    goVectorf n0 (n0_, 3, 1);
    goVectorf n1 (n1_, 3, 1);
    goVectorf b0 (b0_, 3, 1);
    goVectorf b1 (b1_, 3, 1);

    goSinglePlot plot;
    goMultiPlotter mp(1,1);

    plot.setPrefix ("set xlabel 'x'\n set ylabel 'y'\n set zlabel 'z'\n");

    plot.addPlane (n0, b0, 0.1, 0.1, 2.0, 2.0);


    for (n1[0] = -1.0; n1[0] < 1.0; n1[0] += 0.3)
    {
        for (n1[1] = -1.0; n1[1] < 1.0; n1[1] += 0.3)
        {
            // n1 *= 1.0 / n1.norm2();
            goVectorf cut (3);
            if (!goMath::planeLineCut (n0,b0,n1,b1,cut))
            {
                printf ("Failed.\n");
            }

            plot.addPoint (cut, "", "w p ps 2 pt 1 lc rgb \"#00ffff\"");
            goMatrixf line(10,3);
            goDouble l = 2.0;
            for (goSize_t i = 0; i < line.getRows(); ++i)
            {
                goVectorf ref;
                line.refRow(i, ref);
                ref = b1 + n1 * ((float)i * l / (float)(line.getRows() - 1) - 0.5 * l);
            }
            plot.addCurveMatrix (line, "", "w l lt 2");
        }
    }

    mp.addPlot (plot, 0);
    mp.setPauseFlag (true);
    mp.plot ();

    return 1;
}
