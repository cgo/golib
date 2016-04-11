/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignaloperation.h>
#include <gofileio.h>
#include <goplot.h>
#include <gosignal3d.h>
#include <gosignalhelper.h>
#include <gofunctor.h>
#include <gotimerobject.h>

    class Blah
    {
        public:
        goFloat divit (goFloat s)
        {
            return 255.0f - s;
        };
    };

static inline goFloat blub (goFloat s)
{
    return s * 0.5f;
}

int main ()
{
    goSignal3D<void> image;
    // goFileIO::readImage ("/home/christian/Documents/bilder/Dani/Bilder DigiCam I 050.jpg", &image);
    goFileIO::readImage ("/home/gosch/Documents/images/Leeuw test1-small.JPG", &image);
    goSignal3D<void> fimage;
    fimage.setDataType (GO_FLOAT);
    fimage.make (image.getSize(), image.getSize(), image.getBorderSize (), 1);
    goRGBAtoScalar (&image, &fimage);

    goSignal3D<void> fimage2;
    fimage2.make (&fimage);

    Blah blah;
    goSignalOperation1<goFloat> operation;
    operation.setKernelMethod (goFunction<goFloat, goFloat> (blub));
    // operation.setKernelMethod (goMemberFunction<Blah, goFloat, goFloat> (&blah, &Blah::divit));
    goTimerObject to;
    to.startTimer ();
    // operation (fimage, fimage2);
    fimage2.copy (fimage);
    operation (fimage2);
    to.stopTimer ();
    printf ("Seconds: %f\n", to.getTimerSeconds ());

    goPlot::Gnuplot p;
    p.setPrefix ("set yrange [:] reverse\nset palette grey\nset cbtics\n");
    p.plotImage (fimage);
    p.plotImage (fimage2, "", "w image", 1, 0);
    p.plotPause ();

    printf ("min/max 1: %f %f\n", fimage.getMinimum(), fimage.getMaximum ());
    printf ("min/max 2: %f %f\n", fimage2.getMinimum(), fimage2.getMaximum ());

}
