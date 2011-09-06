/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goresample.h>
#include <gosignal2d.h>

template <class T>
goResample<T>::goResample (goInt32 factor) {
  MX = factor;
  MY = factor;
}

template <class T>
goResample<T>::goResample (goInt32 factorX, goInt32 factorY) {
  MX = factorX;
  MY = factorY;
}

template <class T>
goResample<T>::~goResample () {
}

template <class T>
void
goResample<T>::down (goSignal2D<T>& src, goSignal2D<T>& target) 
{
    T* linePtr;
    T* linePtrStart;
    T* targetLinePtr;
    T* targetLinePtrStart;
    goPtrOffset_t offsetX = src.getOffsetX() * MX;
    goPtrOffset_t offsetY = src.getOffsetY() * MY;
    goPtrOffset_t targetOffsetX = target.getOffsetX();
    goPtrOffset_t targetOffsetY = target.getOffsetY();

    register goSize_t height = (goSize_t)(src.getSizeY() / (float)MY);
    register goSize_t width  = (goSize_t)(src.getSizeX() / (float)MX);
    register goSize_t x, y;

    linePtr = linePtrStart = src.getPtr (0,0);
    targetLinePtr = targetLinePtrStart = target.getPtr (0,0);
    for (y = 0; y < height; y++) 
    {
        for (x = 0; x < width; x++) 
        {
            *targetLinePtr = *linePtr;
            linePtr += offsetX;
            targetLinePtr += targetOffsetX;
        }
        linePtr = linePtrStart = linePtrStart + offsetY;
        targetLinePtr = targetLinePtrStart = targetLinePtrStart + targetOffsetY;
    }
}

template <class T>
void
goResample<T>::up (goSignal2D<T>& src, goSignal2D<T>& target) 
{
    T* linePtr;
    T* linePtrStart;
    T* targetLinePtr;
    T* targetLinePtrStart;
    goPtrOffset_t offsetX = src.getOffsetX();
    goPtrOffset_t offsetY = src.getOffsetY();
    goPtrOffset_t targetOffsetX = target.getOffsetX();
    goPtrOffset_t targetOffsetY = target.getOffsetY();

    register goSize_t height = src.getSizeY();
    register goSize_t width  = src.getSizeX();
    register goSize_t x, y;
    register goInt32 i;

    linePtr = linePtrStart = src.getPtr (0,0);
    targetLinePtr = targetLinePtrStart = target.getPtr (0,0);
    for (y = 0; y < height; y++) 
    {
        for (x = 0; x < width; x++) 
        {
            *targetLinePtr = *linePtr;
            linePtr += offsetX;
            targetLinePtr += targetOffsetX;
            for (i = 1; i < MX; i++) 
            {
                *targetLinePtr = (T)0;
                targetLinePtr += targetOffsetX;
            }
        }
        linePtr = linePtrStart = linePtrStart + offsetY;
        targetLinePtr = targetLinePtrStart = targetLinePtrStart + targetOffsetY * MY;
    }
}


template class goResample<goInt32>;
template class goResample<goDouble>;
template class goResample<goFloat>;





