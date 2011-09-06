/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>
#include <goplot.h>

int main ()
{
    goFloat data[] = {0, 0, 1,
                      0, 1, 0};
    goMatrixf simplex (data, 2, 3);
    goVectorf point (2);
    point[0] = 0.76;
    point[1] = 0.1;

    goVectorf barycentric (3);
    goMath::euclideanToBarycentric (simplex, point, barycentric);

    printf ("barycentric size: %d\n", barycentric.getSize());
    printf ("barycentric: %f %f %f\n", barycentric[0], barycentric[1], barycentric[2]);

    goMath::barycentricToEuclidean (simplex, barycentric, point);
    printf ("Point size: %d\n", point.getSize());
    printf ("Point: %f %f\n", point[0], point[1]);

    goVectorf b (3);
    goFloat step = 0.09;
    goString plotdata = "plot \"-\" using 1:2 with points\n";
    for (b[0] = 0.0; b[0] <= 1.0; b[0] += step)
    {
        for (b[1] = 0.0; b[1] <= (1.0 - b[0]); b[1] += step)
        {
            for (b[2] = 0.0; b[2] <= (1.0 - b[0] - b[1]); b[2] += step)
            {
                goVectorf point;
                goMath::barycentricToEuclidean (simplex, b, point);
                plotdata += (float)point[0]; plotdata += " "; plotdata += (float)point[1];
                plotdata += " ";
                plotdata += (float)b[0];
                plotdata += " ";
                plotdata += (float)b[1];
                plotdata += " ";
                plotdata += (float)b[2];
                plotdata += "\n";
            }
        }
    }
    plotdata += "e\n";

    goGnuplot gp;
    gp.call (plotdata);
    
    char c;
    scanf ("%c", &c);

    return 1;
}
