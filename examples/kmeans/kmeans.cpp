/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
    goPlotter plotter;
    goList<goPointf> meansCurve;
    for (i = 0; i < K; ++i)
    {
        if (drawLists[i].getSize() > 0)
        {
            goString title = "Cluster ";
            title += (int)i;
            plotter.addCurve (drawLists[i],title.toCharPtr(),"with points");
        }
        meansCurve.append(goPointf(kmeans.getMeans()[i][0],kmeans.getMeans()[i][1]));
    }
    plotter.addCurve(meansCurve,"Means","with points");
    plotter.setPauseFlag(true);
    plotter.plot();

    exit(1);
}
