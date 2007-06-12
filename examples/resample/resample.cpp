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
        goPlotter plotter;
        plotter.addCurve (x,y,"sin(x)");

        goMatrixd M2;
        goMatrixd temp;
        goResampleLinear (M, temp, 99);
        goResampleLinear (temp, M2, 99);
        M2.refColumn (0,x);
        M2.refColumn (1,y);
        plotter.addCurve (x,y,"sin(x) resampled","with points pointsize 3");
        
        plotter.setPauseFlag (true);
        plotter.plot ();
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
        curve.readASCII (f);
        fclose(f);
        goCurvef curve2;
        curve.resample (10,curve2);

        {
            goPlotter plotter;
            plotter.addCurve (curve.getPoints(),"curve");
            plotter.setPauseFlag(true);
            plotter.plot();
            plotter.addCurve (curve2.getPoints(),"resampled curve","with linespoints");
            plotter.plot();
        }
        exit(1);
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
