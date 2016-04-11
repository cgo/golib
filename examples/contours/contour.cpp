/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gocontours.h>
#include <gomath.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <goplot.h>

// #include <engine.h>
// #include <gomatlab.h>

#include <goshape.h>
#include <goshapehelper.h>

int main (int argc, char* argv[])
{
    if (argc < 3)
    {
        printf ("Usage: %s <image file> <level>\n", argv[0]);
        exit (-1);
    }
    goSignal3D<void> image2;
    // goFileIO::readImage ("/home/gosch/Documents/images/zebra.jpg", &image2);
    //goFileIO::readImage ("/home/gosch/Documents/partial-shapes/ContourTracking/SampleSphere/m1154/m11540000008.jpg", &image2);
    //goFileIO::readImage ("/home/christian/Work/partial-shapes/ContourTracking/SampleSphere/image_00000.jpg", &image2);
    try
    {
        goFileIO::readImage (argv[1], &image2);
    }
    catch (goException& ex)
    {
        printf ("Exception --- does the image exist?\n");
        exit (-1);
    }
    goSignal3D<void> image;
    image.setDataType (GO_FLOAT);
    image.make (image2.getSize(), image2.getSize(), image2.getBorderSize(), 1);
    goRGBAtoScalar (&image2, &image);
    
    goMath::Contours contours;
    goDouble level = atof(argv[2]);
    contours.calculate (image, level);
    goList<goMatrixd>& c = contours.getContours();

    goPlot::Gnuplot plot;
    goList<goMatrixd>::Element* el = c.getFrontElement();
    while (el)
    {
        plot.plot (el->elem, "");
        // printf ("Found matrix size %d %d\n", el->elem.getRows(), el->elem.getColumns());
        el = el->next;
    }
    plot.getGnuplot().call ("set terminal postscript eps color\nset output 'test.eps'\n");
    plot.plot ();

    // printf ("Starting matlab\n");
    // goMatlab matlab;
    // printf ("...done.\n");
    // {
    //     matlab.putSignal (&image, "image");
    //     matlab.putDouble (level, "level");
    //     matlab.call ("contour (image, [level level]);");
    // }
    
    // goShape<goDouble> shape;
    // goExtractShape (image, level, shape, matlab);
    // printf ("Shapes: %d\n", shape.getCurves().getSize());
    // printf ("Contours: %d\n", c.getSize());
    // shape.writeASCII ("shape.txt");

    plot.plotPause();
    return 1;
}
