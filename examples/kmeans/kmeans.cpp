#include <gokmeans.h>
#include <govector.h>
#include <gorandom.h>
#include <goplot.h>

int main (int argc, char* argv[])
{
    goRandom(true);
    goKMeans<goVectord> kmeans;
    goIndex_t K = atoi(argv[1]);
    goFixedArray<goVectord> initmeans (K);

    goIndex_t i;
    for (i = 0; i < 20; ++i)
    {
        goVectord v (2);
        v(0) = (goRandom() - 0.5) * 2.0 + 0.0;
        v(1) = (goRandom() - 0.5) * 2.0 + 1.0;
        kmeans.addElement(v);
    }
    for (i = 0; i < 20; ++i)
    {
        goVectord v (2);
        v(0) = (goRandom() - 0.5) * 2.0 - 1.0;
        v(1) = (goRandom() - 0.5) * 2.0 - 0.0;
        kmeans.addElement(v);
    }
    for (i = 0; i < 20; ++i)
    {
        goVectord v (2);
        v(0) = (goRandom() - 0.5) * 2.0 - 1.0;
        v(1) = (goRandom() - 0.5) * 2.0 - 2.0;
        kmeans.addElement(v);
    }

    {
        goList<goVectord>::ConstElement* el = kmeans.getElements().getFrontElement();
        for (i = 0; i < K && el; ++i)
        {
            initmeans(i) = el->elem;
            el = el->next;
        }
        kmeans.initialise(initmeans);
    }
    goSize_t N = 1;
    while (N != 0)
    {
        N = kmeans.assignment();
        kmeans.update();
        printf ("assignment: %d changed.\n",N);
    }

    //= Plot the results.
    goList<goVectord>::ConstElement* el = kmeans.getElements().getFrontElement();
    goIndex_t sz = kmeans.getElements().getSize();
    i = 0;
    goFixedArray<goList<goPointf> > drawLists(K);
    while (el && i < sz)
    {
        drawLists[kmeans.getCluster()[i]].append (goPointf(el->elem[0],el->elem[1]));
        ++i;
        el = el->next;
    }
    goMultiPlotter plotter(1,1);
    goSinglePlot plot;
    goList<goPointf> meansCurve;
    for (i = 0; i < K; ++i)
    {
        if (drawLists[i].getSize() > 0)
        {
            goString title = "Cluster ";
            title += (int)i;
            plot.addCurve (drawLists[i],title.toCharPtr(),"with points");
        }
        meansCurve.append(goPointf(kmeans.getMeans()[i][0],kmeans.getMeans()[i][1]));
    }
    plot.addCurve(meansCurve,"Means","with points");
    plot.setPrefix (goString("set key on outside\n"));
    plotter.setPauseFlag(true);
    plotter.addPlot (plot, 0, 0);
    plotter.plot();

    exit(1);
}
