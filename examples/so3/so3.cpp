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

void karcher_test ()
{
    goMath::SO3<goFloat> so3;
    goFixedArray <goMath::SO3<goFloat>::Element> elements (10);

    goSinglePlot plot;

    goMath::SO3<goFloat>::Tangent t, t2;
    t[0] = 0.3;
    t[1] = 0.3;
    t[2] = 0.7;
    goRandom (true);
    t2 = t;
    for (goSize_t i = 0; i < elements.getSize(); ++i)
    {
//        t2[0] = t[0] + (0.5f - goRandom ()) * 1.0f;
        t2[1] = t[1] + (0.5f - goRandom ()) * 1.0f;
        t2[2] = t[2] + (0.5f - goRandom ()) * 1.0f;

        so3.matrix (t2, elements[i]);
        plot_tripod (plot, elements[i], 1);
    }

    goMultiPlotter mp (1,1);
    goString prefix = "set style line 1 lt 1 lc rgb 'red'\nset style line 2 lt 1 lc rgb 'green'\nset style line 3 lt 1 lc rgb 'blue'\nset style line 4 lt 1 lc rgb 'magenta'\nset style line 5 lt 1 lc rgb 'yellow'\nset style line 6 lt 1 lc rgb 'black'\n";
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
}

int main ()
{
    karcher_test ();
    exit (1);

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
