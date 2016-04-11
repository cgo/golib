/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot/gnuplot.h>
#include <gorandom.h>
#include <gomath.h>
#include <gosignal3d.h>
#include <gofileio.h>

int main ()
{
    
    {
        goMultiPlotter mp (1,1);
        goSinglePlot plot;
        plot.setPrefix ("set size square\n");
        plot.setUseFiles (true);
        goVectorf phi (10), theta (10);
        phi.fillRange(-4.0, 1.0, 5.0);
        phi *= M_PI / (float)(phi.getSize() / 2 - 1);
        theta.fillRange(0.0, 1.0, 10.0);
        theta *= M_PI / (float)(theta.getSize() - 1);
        for (goSize_t i = 0; i < phi.getSize(); ++i)
        {
            for (goSize_t j = 0; j < theta.getSize(); ++j)
            {
                goVectorf n;
                goVectorf p(3); p.fill (0.0);
                goMath::sphereToEuclidean<goFloat> (phi[i], theta[j], 1.0, &n, 0);
                plot.addLine (n, p);
            }
        }
        mp.addPlot (plot,0);
        mp.setPauseFlag (true);
        mp.plot ();
    }
    {
        goMultiPlotter mp (1,1);
        goSinglePlot plot;
        plot.setPrefix ("set size square\n");
        plot.setUseFiles (true);
        goVectorf sinx (100), cosx(100);
        sinx.fillRange(0.0, 1.0, 100.0);
        sinx *= 2 * M_PI / 99.0;
        cosx = sinx;
        goMath::sin (sinx);
        goMath::cos (cosx);
        for (goSize_t i = 0; i < cosx.getSize(); ++i)
        {
            goVectorf n (2);
            goVectorf p (2); p.fill (0.0);
            n[0] = sinx[i];
            n[1] = cosx[i];
            plot.addLine (n, p);
        }
        mp.addPlot (plot,0);
        mp.setPauseFlag (true);
        mp.plot ();
        exit (1);
    }

    goFixedArray<goFloat> a1 (5);
    a1[0] = 1.0;
    a1[1] = 2.0;
    a1[2] = 0.5;
    a1[3] = 3.0;
    a1[4] = 2.0;
    goFixedArray<goFloat> b1 (5);
    b1[0] = 10.0;
    b1[1] = 11.0;
    b1[2] = 12.5;
    b1[3] = 13.0;
    b1[4] = 14.0;
    goFixedArray<goFloat> b2 (5);
    b2[0] = 5.0;
    b2[1] = 6.0;
    b2[2] = 7.5;
    b2[3] = 8.0;
    b2[4] = 9.0;
    goFixedArray<goFloat> a2 (5);
    a2[0] = 1.0 + 5;
    a2[1] = 2.0 + 5;
    a2[2] = 0.5 + 5;
    a2[3] = 3.0 + 5;
    a2[4] = 2.0 + 5;

    goList<goFixedArray<goFloat> > l;
    goList<goFixedArray<goFloat> > l2;
    l.append(a1);
    l.append(a2);
    l2.append(b1);
    l2.append(b2);
    goList<goString> titles;
    titles.append(goString("a1"));
    titles.append(goString("a2"));
    goList<goString> plotCommands;
    plotCommands.append("with lines");
    plotCommands.append("with lines");

    goPlot::gnuplotList (&l2,&l,&titles,&plotCommands,0,"pause -1\n");

    goFixedArray<goDouble> x (100);
    goFixedArray<goDouble> y (100);
    
    goSize_t i;
    goRandom(true);
    for (i = 0; i < x.getSize(); ++i)
    {
        x[i] = i;
        y[i] = goRandom();
    }

    goDouble mean = goMath::fastMean<goFixedArray<goDouble>,goDouble>(y,y.getSize());
    goDouble variance = goMath::fastVariance<goFixedArray<goDouble>,goDouble>(y,y.getSize(),mean);
    goFixedArray<goDouble> m (100);
    m.fill (mean);
    goFixedArray<goDouble> d_plus (100);
    goFixedArray<goDouble> d_minus (100);
    d_plus.fill (mean + sqrt(variance));
    d_minus.fill (mean - sqrt(variance));
    
    goPlotter plotter;
    plotter.addCurve(x,y,"random");
    plotter.addCurve(x,m,"mean");
    plotter.addCurve(x,d_plus,"mean + deviation");
    plotter.addCurve(x,d_minus,"mean - deviation");
    plotter.setPauseFlag(true);
    // plotter.plot();
  
    {
        printf ("Testing goPlot::Plot\n");
        goPlot::Gnuplot p;
        p.plot (x,y);
        p.plot ();
        char c;
        scanf ("%c",&c);
        scanf ("%c",&c);
        scanf ("%c",&c);
        scanf ("%c",&c);
        scanf ("%c",&c);
        scanf ("%c",&c);
        scanf ("%c",&c);
    }

    {
        printf ("Testing goMultiPlotter\n");
        goMultiPlotter mp (2,3);
        goSinglePlot sp1;
        sp1.addCurve(x,y,"random");
        sp1.addCurve(x,m,"mean");
        mp.addPlot (sp1,0,0);
        sp1.clear ();
        sp1.addCurve(x,d_plus,"mean + deviation");
        sp1.addCurve(x,d_minus,"mean - deviation");
        mp.addPlot (sp1,1,1);
        mp.setPauseFlag (true);
        // mp.plot ();
        // mp.plotPostscript ("test.ps");
    }

    {
        goSinglePlot plot;
        goSize_t N = 20;
        goSize_t M = 20;
        goVectord x (N*M);
        goVectord y (N*M);
        goVectord z (N*M);

        for (goSize_t i = 0; i < N; ++i)
        {
            for (goSize_t j = 0; j < M; ++j)
            {
                x[i+j*N] = (float)i;
                y[i+j*N] = (float)j;
                z[i+j*N] = (float)sin(M_PI*i/(float)N*M_PI*j/(float)M);
            }
        }

        plot.add3D (x,y,N,z,"My plot");
        goMultiPlotter plotter (1,1);
        plotter.addPlot (plot,0,0);
        plotter.setPauseFlag (true);
        plotter.plot ();
        plotter.setPauseFlag (false);
        plotter.plotEPS ("test.eps");
    }

    {
        goSinglePlot plot;

        goSignal3D<void> image;
        goFileIO::readImage ("/home/christian/tisch2.jpg", &image);
        
        plot.setTitle ("A surface");
        plot.add3D (&image, "My image");
        goMultiPlotter plotter (1,2);
        plotter.addPlot (plot,0,0);
        
        goSinglePlot sp1;
        sp1.setTitle ("A random curve");
        sp1.addCurve(x,y,"random");
        sp1.addCurve(x,m,"mean");
        plotter.addPlot (sp1,0,1);
        
        plotter.setPauseFlag (true);
        plotter.plot ();
    }

    exit(1);
}
