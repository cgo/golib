/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gomath.h>
#include <goplot.h>
#include <gorandom.h>

template<class T>
void plot_tripod (goSinglePlot& p, goMatrix<T>& R, int style_offset)
{
    goVector<T> o (3);
    o.fill(0.0);
    for (goIndex_t i = 0; i < 3; ++i)
    {
        goVector<T> r;
        R.refRow (i, r);
        goString cmd = "w l ls "; cmd += int(i) + style_offset;
        p.addLine (r, o, "", cmd.toCharPtr());
    }
}


int main ()
{
    goMath::SO3<goFloat> so3;
    goVector<goFloat> w(3);
    w[0] = 1.0; w[1] = 0.0; w[2] = 0.0;
    w *= M_PI * 0.5;
    goMatrixf R1;
    so3.matrix (w, R1);

    w[0] = 1.0; w[1] = 1.0; w[2] = 2.0;
    w *= M_PI;
    
    goMatrixf R2;
    so3.matrix (w, R2);

    R1.print();
    R2.print();

    goMath::SO3<goFloat>::Element R;
    goMath::SO3<goFloat>::Tangent v;

    //=
    //= Calculate the tangent space representation of R2 with
    //= respect to R1
    //=
    so3.log (R1, R2, v);

    goGnuplot gp;
    goMultiPlotter mp (1,1);
    goSinglePlot plot;
    goString prefix ="\n\
        set style line 1 lw 4 lt 1 lc rgb 'red'\n\
        set style line 2 lw 4 lt 1 lc rgb 'green'\n\
        set style line 3 lw 4 lt 1 lc rgb 'blue'\n\
        set style line 4 lw 1 lt 1 lc rgb 'red'\n\
        set style line 5 lw 1 lt 1 lc rgb 'green'\n\
        set style line 6 lw 1 lt 1 lc rgb 'blue'\n\
        set mouse\n";
    mp.setPrefix (prefix);
    plot_tripod (plot, R1, 4);
    plot_tripod (plot, R2, 1);

    (R1*R1.getTranspose()).print();
    (R2*R2.getTranspose()).print();

    //= 
    //= Calculate a geodesic from R1 to R2
    //=
    for (goIndex_t i = 0; i < 10; ++i)
    {
        goFloat t = float(i) / 9.0;
        so3.exp (R1, v * t, R);
        R.print ();
        plot_tripod (plot, R, 4);
        mp.clear();
        mp.setPrefix (prefix);
        mp.addPlot (plot,0);
        mp.setPostfix ("pause mouse\n");
        mp.plot(&gp);
    }

    exit (1);
}
