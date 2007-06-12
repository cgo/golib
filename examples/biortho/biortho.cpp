#include <goplot.h>
#include <gobiorthowavelet.h>
#include <gosignal3d.h>
#include <gocurve.h>
#include <stdio.h>

void plot (goSignal3DBase<void>& sig, const char* title)
{
    printf ("Plotting signal %s of type %s\n", sig.getObjectName().toCharPtr(), sig.getDataType().getString().toCharPtr());
    assert (sig.getDataType().getID() == GO_FLOAT);
    goArray<goFloat> a (sig.getSizeX());
    goIndex_t i;
    for (i = 0; i < a.getSize(); ++i)
    {
        a[i] = *(goFloat*)sig.getPtr(i,0,0);
    }
    goString temp1;
    goString temp2;
    goPlot::gnuplot (a, temp1, temp2, title);
}

int main (int argc, char* argv[])
{
    goCurvef curve;
    FILE* f = fopen (argv[1],"r");
    if (!f)
    {
        printf ("Could not read file %s\n", argv[1]);
        return 2;
    }
    if (!curve.readObjectFile (f))
    {
        printf ("Could not read file %s\n", argv[1]);
        return 2;
    }

    goList<goPointf>* points = &curve.getPoints();
    goSignal3D<void> X; 
    X.setDataType (GO_FLOAT);
    X.make (points->getSize(), 1, 1, points->getSize(), 1, 1, 8, 0, 0);
    goList<goPointf>::Element* el = points->getFrontElement();
    goIndex_t i = 0;
    while (el && i < X.getSizeX()) 
    {
        *(goFloat*)X.getPtr(i,0,0) = el->elem.x;
        el = el->next;
        ++i;
    }

    goBiorthoWavelet biortho;
    goSignal3D<void> hi;
    goSignal3D<void> lo;
    if (!biortho.transform (X, hi, lo))
    {
        printf ("Biortho transform failed!\n");
        return 2;
    }
    
    //plot (hi, "High");
    //plot (lo, "Low");
    plot (X, "X");

    goSignal3D<void> lo2;
    goSignal3D<void> hi2;
    biortho.transform (lo, hi2, lo2);
    //plot (hi2, "High 2");
    //plot (lo2, "Low 2");

    goSignal3D<void> newLo;
    biortho.inverse (hi2, lo2, newLo);
    
    goSignal3D<void> recon;
    biortho.inverse (hi, newLo, recon);
    plot (recon, "Reconstruction");
    
    recon -= X;
    plot (recon, "Difference");

    plot (newLo, "Low 1 reconstructed");
    plot (lo2, "Low 2");
    
    exit (1);
}
