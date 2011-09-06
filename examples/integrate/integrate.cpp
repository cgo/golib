/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>
#include <goplot.h>

int main ()
{
    goVectord sinx (100);
    sinx.fillRange (0,1,100);
    sinx *= 1.0 * M_PI / 99.0;
    goMath::sin (sinx);

    goPlot::plot (sinx, "sinx");

    printf ("Trapez: %lf\n", goMath::integrate(sinx));
    printf ("Simpson: %lf\n", goMath::integrateSimpson(sinx));

    return 1;
}
