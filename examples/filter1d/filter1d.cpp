/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gosignal3d.h>
#include <gofilter1d.h>
#include <gofileio.h>
#include <stdio.h>

int main (int argc, char* argv[])
{
    {
        goSignal3D<void> signal;
        signal.setDataType (GO_UINT8);
        signal.make (9,9,1,4,4,1,4,4,0,1);
        const goPtrdiff_t* xj = signal.getXJump() - 4;
        const goPtrdiff_t* xd = signal.getXDiff() - 4;
        const goPtrdiff_t* yj = signal.getYJump();
        const goPtrdiff_t* zj = signal.getZJump();
        int i;
        for (i = 0; i < (signal.getSizeX() + 8); ++i)
        {
            printf ("%d ", *xj);
            ++xj;
        }
        printf("\n");
        for (i = 0; i < (signal.getSizeX() + 8); ++i)
        {
            printf ("%d ", *xd);
            ++xd;
        }
        printf("\n");
        // return 0;
    }
    
    goSignal3D<void> signal;
    if (goFileIO::readImage (argv[1], &signal) == false)
    {
        std::cout << "Could not read image.\n";
        return 2;
    }
    signal.setDataType (GO_INT8);
    signal.make (1600,1200,1,32,32,1,32,32,0,4);
    goFilter1D filter;
    goArray<goFloat> mask;
    mask.resize(3); mask[0] = 1.0f; mask[1] = 2.0f; mask[2] = 1.0f;
    filter.setMask(mask);
    filter.setCenter(1);
    filter.normalize();
    signal.rotateAxes();
    signal.rotateAxes();
//    signal.rotateAxes();
    if (signal.getSizeX() > 1)
        filter.filter(signal);
    if (signal.getSizeX() > 1)
        filter.filter(signal);
    if (signal.getSizeX() > 1)
        filter.filter(signal);
    return 0;
}
