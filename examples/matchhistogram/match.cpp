/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal3d.h>
#include <gohistogram.h>
#include <gofileio.h>
#include <gostring.h>
#include <gosignalhelper.h>
#include <gosignalmacros.h>
#include <goplot.h>

int main (int argc, char* argv[])
{
    goString bildfile1 = argv[1];
    goString bildfile2 = argv[2];

    goSignal3D<void> bild1;
    goSignal3D<void> bild2;

    goSignal3D<void> temp;
    goFileIO::readImage (bildfile1.toCharPtr(), &temp);
    bild1.setDataType (GO_FLOAT);
    bild1.make (&temp);
    if (!goCopySignal (&temp, &bild1))
    {
        printf ("convert 1 failed\n");
    }
    goFileIO::readImage (bildfile2.toCharPtr(), &temp);
    bild2.setDataType (GO_FLOAT);
    bild2.make (&temp);
    if (!goCopySignal (&temp, &bild2))
    {
        printf ("convert 2 failed\n");
    }
  
    temp.setDataType (GO_FLOAT);
    temp.make (&bild1);

    goCDF<goFloat> cdf;
    cdf.setBins (256);
    cdf.calculate (bild2);

    goCDF<goFloat> cdfbild1;
    cdfbild1.setBins (256);
    cdfbild1.calculate (bild1);

    goEqualizeHistogram (&bild1, cdf);

    goPlotter plotter;
    plotter.addCurve (cdf.getHistogram(), "bild2");
    cdf.calculate (bild1);
    plotter.addCurve (cdf.getHistogram(), "matched bild1");
    plotter.addCurve (cdfbild1.getHistogram(), "original bild1");
    plotter.setPauseFlag (true);
    plotter.plot ();

    //if (!goMatchHistograms (&bild1, &bild2, &temp))
    //{
    //    printf ("Match failed.\n");
    //}

//    GO_SIGNAL3D_EACHELEMENT_GENERIC(if (*(goFloat*)__ptr > 255.0f) *(goFloat*)__ptr = 254.9f;\
//            if (*(goFloat*)__ptr < 0.0f) *(goFloat*)__ptr = 0.0f;, temp);

    temp.setDataType (GO_UINT8);
    temp.make (&bild1);
    if (!goCopySignal (&bild1, &temp))
    {
        printf ("convert 3 failed\n");
    }

    try
    {
        goFileIO::writeImage (argv[3], &temp);
    }
    catch (goFileIOException& ex)
    {
        if (ex.code == goFileIOException::EXISTS)
        {
            printf ("BAAAAH exists\n");
        }
        if (ex.code == goFileIOException::FAILED)
        {
            printf ("BAAAAH failed\n");
        }
    }

    return 1;
}
