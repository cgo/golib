#include <stdlib.h>
#include <stdio.h>

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

/*
 * @brief Karcher mean for rotations.
 *
 * Generates 10 rotation matrices using random tangent vectors from one
 * central rotation,
 * then uses karcherMean() to find a mean rotation.
 * Plots rotations using tripods before and after mean calculations.
 */
void karcher_test_so3 ()
{
    goMath::SO3<goFloat> so3;
    goFixedArray <goMath::SO3<goFloat>::Element> elements (10);

    goSinglePlot plot;

    goMath::SO3<goFloat>::Tangent t(3), t2(3);
    t[0] = 1.0;
    t[1] = 1.0;
    t[2] = 1.0;
    goMath::SO3<goFloat>::Element M;
    so3.matrix (t, M);
    goRandom (true);
    for (goSize_t i = 0; i < elements.getSize(); ++i)
    {
        t2[0] = (0.5f - goRandom ()) * 1.0f;
        t2[1] = (0.5f - goRandom ()) * 1.0f;
        t2[2] = (0.5f - goRandom ()) * 1.0f;

        so3.exp (M, t2, elements[i]);
        plot_tripod (plot, elements[i], 1);
    }

    goMultiPlotter mp (1,1);
    goString prefix = "\
        set style line 1 lt 2 lw 2 lc rgb \"#700000\"\n\
        set style line 2 lt 2 lw 2 lc rgb \"#007000\"\n\
        set style line 3 lt 2 lw 2 lc rgb \"#000070\"\n\
        set style line 4 lt 1 lw 8 lc rgb \"#ff5000\"\n\
        set style line 5 lt 1 lw 8 lc rgb \"#50ff00\"\n\
        set style line 6 lt 1 lw 8 lc rgb \"#0050ff\"\n";
    mp.setPrefix (prefix.toCharPtr());
    mp.addPlot (plot, 0);
    mp.setPauseFlag (true);
    mp.plot ();

    goMath::SO3<goFloat>::Element mean;
    if (!goMath::karcherMean (&elements[0], elements.getSize(), so3, mean))
    {
        printf ("Mean calculation failed!\n");
    }
    plot_tripod (plot, mean, 4);
    mp.clear ();
    mp.setPrefix (prefix.toCharPtr());
    mp.setPauseFlag (true);
    mp.addPlot (plot, 0);
    mp.plot ();
    mp.saveGnuplot ("meanrot.gnuplot");
}

void karcher_test_linear ()
{
    goMath::LinearSpace<goFloat> ls;
    goFixedArray <goVectorf> elements (10);

    for (goSize_t i = 0; i < elements.getSize(); ++i)
        elements[i].resize (2);

    goSinglePlot plot;

    goVectorf t (2);
    goVectorf M (2);
    M[0] = 10.0f;
    M[1] = 10.0f;

    goRandom (true);
    for (goSize_t i = 0; i < elements.getSize(); ++i)
    {
        t[0] = (0.5f - goRandom ()) * 1.0f;
        t[1] = (0.5f - goRandom ()) * 1.0f;

        ls.exp (M, t, elements[i]);
        plot.addPoint (elements[i], "", "w p ls 1");
        // plot_tripod (plot, elements[i], 1);
    }

    goMultiPlotter mp (1,1);
    goString prefix = "\
        set style line 1 lt 2 pt 1 ps 2 lw 2 lc rgb \"#0000ff\"\n\
        set style line 2 lt 1 pt 2 ps 4 lw 4 lc rgb \"#ff0000\"\n\
        # set yrange [8.0:12.0]\n\
        # set xrange [8.0:12.0]\n\
        unset border\n\
        unset tics\n";
    mp.setPrefix (prefix.toCharPtr());
    mp.addPlot (plot, 0);
    mp.setPauseFlag (true);
    mp.plot ();

    goVectorf mean;
    if (!goMath::karcherMean (&elements[0], elements.getSize(), ls, mean))
    {
        printf ("Mean calculation failed!\n");
    }
    plot.addPoint (mean, "", "w p ls 2");
    mp.clear ();
    mp.setPrefix (prefix.toCharPtr());
    mp.setPauseFlag (true);
    mp.addPlot (plot, 0);
    mp.plot ();
    mp.saveGnuplot ("meanlin.gnuplot");
}

int main ()
{
    printf ("Karcher mean example using golib.\n(C) Christian Gosch\n");
    printf ("Generates meanrot.gnuplot for use in Gnuplot.\n");
    // karcher_test_so3 ();
    karcher_test_linear ();
    exit (1);
}
