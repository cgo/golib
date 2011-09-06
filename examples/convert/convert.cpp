/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal3d.h>
#include <gosignal3dgenericiterator.h>
#include <gofileio.h>
#include <gosignalhelper.h>
#include <stdio.h>

int main (int argc, char* argv[])
{
    goSignal3D<void> image;
    image.setDataType(GO_UINT16);
    if (!goFileIO::readImage (argv[1], &image))
    {
        printf ("Buhu\n");
        exit(2);
    }
    goSignal3D<void> converted;
    converted.setDataType(GO_UINT16);
    converted.make (&image);
    if (!goRGBAtoScalar(&image, &converted))
    {
        printf ("Could not convert.\n");
        exit(2);
    }

    goString info;
    goSignalInfoText (converted, info);
    printf ("%s\n",info.toCharPtr());
    printf ("Maximum: %f, Minimum: %f\n", converted.getMaximum(), converted.getMinimum());

    for (int i = 0; i < image.getChannelCount(); ++i)
    {
        image.setChannel (i);
        printf ("Channel %d Maximum: %f, Minimum: %f\n", i, converted.getMaximum(), converted.getMinimum());
    }
    
    goSignal3DGenericIterator it (&converted);
    FILE* f = fopen (argv[2],"w");
    if (!f)
    {
        printf ("Could not open output file.\n");
        exit(2);
    }
    while (!it.endY())
    {
        it.resetX();
        while (!it.endX())
        {
            fwrite (*it, sizeof(goUInt16), 1, f);
            it.incrementX();
        }
        it.incrementY();
    }

    fclose(f);
    exit(1);

    try
    {
        goFileIO::writeImage (argv[2], &converted);
    }
    catch (goFileIOException& ex)
    {
        printf ("Could not write.\n");
        exit(2);
    }
    exit(1);
}
