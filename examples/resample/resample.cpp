/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goplot.h>
#include <gocurve.h>
#include <gotypes.h>
#include <golist.h>
#include <gofixedarray.h>
#include <goresample.h>

#include <stdlib.h>
#include <stdio.h>

int main (int argc, char* argv[])
{
    bool do_sine_test = true;
    if (do_sine_test)
    {
        goMatrixd M (10, 2);
        goDouble t = 0.0;
        goDouble dt = M_PI / (float)(M.getRows()-1);
        goSize_t i;
        for (i = 0; i < M.getRows(); ++i, t += dt)
        {
            M(i,0) = t;
            M(i,1) = sin(t);
        }

        goVectord x;
        goVectord y;
        M.refColumn (0,x);
        M.refColumn (1,y);
        goMultiPlotter mp(1,1);
        goSinglePlot plot;
        plot.addCurve (x,y,"sin(x)","with points ps 4");

        goMatrixd M2;
        goMatrixd M3;
        goMatrixd temp;
        goMath::resampleCubic (M, M2, 99, false);
        goMath::resampleCubic (M, M3, 99, true);

        M2.writeASCII ("M2.txt");
        M3.writeASCII ("M3.txt");

        // goResampleLinear (M, temp, 99);
        // goResampleLinear (temp, M2, 99);
        plot.addCurveMatrix (M2,"sin(x) resampled","with points pointsize 1");
        plot.addCurveMatrix (M3,"sin(x) resampled, closed","with points pointsize 1");
        plot.setPrefix ("set key on above\n");
        mp.addPlot(plot,0);
        mp.setPauseFlag (true);
        mp.plot ();
        exit (1);
    }
    
    if (argc > 1)
    {
        goCurvef curve;
        FILE* f = fopen (argv[1],"r");
        if (!f)
        {
            printf ("Could not open %s for reading.\n",argv[1]);
            exit(-1);
        }
        printf ("Reading curve\n");
        curve.readASCII (f);
        fclose(f);

        printf ("Resampling\n");
        goMatrixf M;
        goMatrixf M2;
        goMatrixf M3;
        curve.getConfigurationMatrix (M);
        goMath::resampleCubic (M, M2, 100, curve.getPoints().isClosed());
        goResampleLinear (M, M3, 100);
        printf ("Done, plotting\n");

        goMultiPlotter mp (1,1);
        goSinglePlot plot;
        plot.addCurveMatrix (M, "Original points", "with lines");
        plot.addCurveMatrix (M2, "Resampled points", "with linespoints");
        plot.addCurveMatrix (M3, "Linearly resampled points", "with linespoints");
        plot.setPrefix ("set key on above\n");
        mp.addPlot (plot,0);
        mp.setPauseFlag (true);
        mp.plot ();


        //goCurvef curve2;
        //curve.resample (10,curve2);


        //{
        //    goPlotter plotter;
        //    plotter.addCurve (curve.getPoints(),"curve");
        //    plotter.setPauseFlag(true);
        //    plotter.plot();
        //    plotter.addCurve (curve2.getPoints(),"resampled curve","with linespoints");
        //    plotter.plot();
        //}
        exit(1);
    }
    else
    {
        printf ("Usage: %s <curve file>\n", argv[0]);
        exit(2);
    }

# if 0
    goCurvef curve;
    goList<goPointf> l;
    const goFloat a[] = {1.0f, 5.0f, 5.0f, 2.0f}; //, 2.0f, 5.0f, 5.0f};
    const goIndex_t n = 4;
    goFixedArray<goFloat> array (n);
    memcpy(array.getPtr(),a,sizeof(goFloat)*n);

    goPlot::gnuplot(array,"a",0,0,0,0,0,false);

    for (goIndex_t i = 0; i < array.getSize(); ++i)
    {
        l.append(goPointf((goFloat)i,array[i]));
    }
    {
        goList<goPointf>::Element* el = l.getFrontElement();
        goDouble dist = 0.0;
        while (el && el->next)
        {
            goDouble temp1 = el->next->elem.x - el->elem.x;
            goDouble temp2 = el->next->elem.y - el->elem.y;
            dist += sqrt(temp1 * temp1 + temp2 * temp2);
            el = el->next;
        }
        printf ("resample.cpp: dist == %f\n", dist);
    }
    
    goList<goPointf> resampled2;
    goCurvef c1;
    goList<goPointf>& resampled = c1.getPoints();
    
    // c1.setPoints(l.getFrontElement(),l.getSize(),10,false);
    goCurvef::resampleLinear(l.getFrontElement(),l.getSize(),10,resampled,false);

    array.setSize(resampled.getSize());
    goList<goPointf>::Element* el = resampled.getFrontElement();
    FILE* f = fopen("out.txt","w");
    for (goIndex_t i = 0; i < resampled.getSize() && el; ++i)
    {
        array[i] = el->elem.y;
        fprintf(f,"%f %f\n",el->elem.x,el->elem.y);
        el = el->next;
    }
    fclose(f);
    printf ("%d points in resampled version.\n",array.getSize());
    goPlot::gnuplot(array,"resampled",0,0,0,0,0,false);
    
    exit(1);
#endif
}
