/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gofileio.h>
#include <gokmeans.h>
#include <gokmeansspatial.h>
#include <gosignal3d.h>
#include <gosignalhelper.h>
#include <gosubsignal3d.h>
#include <gosort.h>
#include <govector.h>
#include <gostring.h>
#include <gorandom.h>

/*
* Example for image segmentation using k-means.
*/
int main (int argc, char* argv[])
{
    if (argc < 5)
    {
        printf ("Usage: %s <image filename> <cluster count> <neighbourhood size x> <neighbourhood size y>\n",argv[0]);
        exit(-1);
    }
    goString filename = argv[1];
    goSize_t K = atoi(argv[2]);
    //= Neighbourhood size
    goSize_t nx = atoi(argv[3]);
    goSize_t ny = atoi(argv[4]);
    //= Read and convert image.
    goSignal3D<void> image;
    {
        goSignal3D<void> temp;
        try
        {
            goFileIO::readImage(filename.toCharPtr(),&temp);
        }
        catch (goFileIOException& ex)
        {
            printf ("IO Exception. Does the file exist?\n");
            exit(2);
        }
        catch (goTypeException& ex)
        {
            printf ("Type Exception. Wrong type?\n");
            exit(2);
        }

        image.setDataType(GO_FLOAT);
        image.make (&temp);
        // image.make(temp.getSize(),temp.getBlockSize(),goSize3D(15,15,0),1);
        if (temp.getChannelCount() > 1)
        {
            // goRGBAtoScalar (&temp,&image);
            goCopySignal (&temp, &image);
        }
        else
        {
            goCopySignal (&temp, &image);
        }
        goNormalizeSignal (&image);
    }

    //= Do both k-means and the augmented k-means with spatial weighting
    goKMeansSpatial<goVectord> kmeansspatial;
    goKMeans<goVectord> kmeans;
    goSize_t N = image.getSizeX() * image.getSizeY();
    goSubSignal3D<void> window (&image, nx, ny, 1);
    goVectord v (nx*ny * image.getChannelCount());
    goSize_t x;
    goSize_t y;
    goDouble imageMax = image.getMaximum();
    goDouble imageMin = image.getMinimum();
    goDouble xPosScale = (imageMax - imageMin) / static_cast<float>(image.getSizeX());// * nx * 1.0;
    goDouble yPosScale = (imageMax - imageMin) / static_cast<float>(image.getSizeY());// * ny * 1.0;
    for (y = 0; y < image.getSizeY(); ++y)
    {
        for (x = 0; x < image.getSizeX(); ++x)
        {
            window.setPosition(x - nx/2,y - ny/2,0);
            for (goSize_t i = 0; i < window.getChannelCount(); ++i)
            {
                window.setChannel(i);
                goCopySignalArray (&window, v.getPtr() + nx*ny*i);
            }
            window.setChannel(0);
            // goSort (v.getPtr(), v.getSize());
            goVectord pos(2);
            pos(0) = imageMin + (float)x * xPosScale;
            pos(1) = imageMin + (float)y * yPosScale;
            //goVectord augmented_v;
            //v.cat(pos, augmented_v);
            // kmeans.addElement (augmented_v);
            kmeans.addElement (v);
            kmeansspatial.addElement (v);
            kmeansspatial.addPosition (pos);
        }
    }
    goFixedArray<goVectord> initMeans (K);
    goFixedArray<goVectord> initMeansSpatial (K);
    goFixedArray<goVectord> initPos (K);
    goList<goVectord>::ConstElement* el = kmeans.getElements().getFrontElement();
    goList<goVectord>::ConstElement* els = kmeansspatial.getElements().getFrontElement();
    goList<goVectord>::ConstElement* elPos = kmeansspatial.getPositions().getFrontElement();
    for (goSize_t i = 0; i < K; ++i)
    {
        el = kmeans.getElements().getFrontElement();
        goSize_t j = (int)(goRandom() * (kmeans.getElements().getSize()-1)) + 1;
        for (;j > 0; --j)
        {
            el = el->next;
        }
        assert (el && elPos && els);
        initMeans[i] = el->elem;
        initMeansSpatial[i] = els->elem;
        initPos[i] = elPos->elem;
        // el = el->next;
        els = els->next;
        elPos = elPos->next;
    }
    //= Initialise k-means
    kmeans.initialise(initMeans);
    kmeansspatial.initialise(initMeansSpatial);
    kmeansspatial.initialisePositions(initPos);
    kmeans.assignment();
    kmeans.update();
    kmeansspatial.assignment();
    kmeansspatial.update();
    goSize_t changes = 1;
    goSize_t changes2 = 0;
    //= Iterate assignment and update steps until no changes in labelling occur
    while (changes > 0 || changes2 > 0)
    {
        changes = kmeans.assignment();
        // changes2 = kmeansspatial.assignment();
        kmeans.update();
        // kmeansspatial.update();
        printf ("Changes, spatial changes: %d, %d\n",changes,changes2);
    }

    //= Write the two label images (note that writeImage() aborts if the file already exists).
    goSignal3D<void> labelImage;
    labelImage.setDataType(GO_UINT8);
    labelImage.make(image.getSize(),image.getBlockSize(),image.getBorderSize(),1);
    goCopySignalArray(((const goKMeans<goVectord>)kmeans).getCluster().getPtr(), &labelImage);
    labelImage *= 255 / (initMeans.getSize());

    goSignal3D<void> labelImageSpatial;
    labelImageSpatial.setDataType(GO_UINT8);
    labelImageSpatial.make(image.getSize(),image.getBlockSize(),image.getBorderSize(),1);
    goCopySignalArray(((const goKMeansSpatial<goVectord>)kmeansspatial).getCluster().getPtr(), &labelImageSpatial);
    labelImageSpatial *= 255 / (initMeansSpatial.getSize());
   
    goString filename2;
    filename.getFileName(filename2);
    filename2 += "-cluster-kmeans.pgm";
    try 
    {
        goFileIO::writeImage(filename2.toCharPtr(),&labelImage);
    }
    catch (goFileIOException& ex)
    {
        if (ex.code == goFileIOException::EXISTS)
        {
            goFileIO::remove(filename2.toCharPtr());
            goFileIO::writeImage(filename2.toCharPtr(),&labelImage);
        }
    }
    filename.getFileName(filename2);
    filename2 += "-cluster-kmeansspatial.pgm";
    try 
    {
        goFileIO::writeImage(filename2.toCharPtr(),&labelImageSpatial);
    }
    catch (goFileIOException& ex)
    {
        if (ex.code == goFileIOException::EXISTS)
        {
            goFileIO::remove(filename2.toCharPtr());
            goFileIO::writeImage(filename2.toCharPtr(),&labelImageSpatial);
        }
    }
    exit(1);
}
