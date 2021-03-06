/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goresample.h>
#include <golog.h>

template <class T>
bool goResampleLinear (const goMath::Matrix<T>& source, goMath::Matrix<T>& target, goSize_t resamplePointCount)
{
    goSize_t pointCount = source.getRows();
    if (pointCount < 2)
    {
        goLog::warning("goResampleLinear(): point count is < 2.");
        return false;
    }
    if (resamplePointCount < 2)
    {
        return false;
    }
    goDouble curveLength = 0.0;

    goFixedArray<goDouble> accumLength (pointCount);
    
    {
        accumLength[0] = 0.0;
        goIndex_t i;
        goMath::Vector<T> row1;
        goMath::Vector<T> row2;
        for (i = 0; i < pointCount - 1; ++i)
        {
            source.refRow (i, row);
            source.refRow (i + 1, row);
            curveLength += (row1 - row2).abs();
            accumLength[i + 1] = curveLength;
        }
    }

    goDouble step = curveLength / (double)(resamplePointCount-1);

    target.resize (resamplePointCount, source.getColumns());

    goDouble t = 0.0f;
    goIndex_t i = 0;
    goIndex_t j = 0;
    goMath::Vector<T> row1;
    goMath::Vector<T> row2;
    goMath::Vector<T> targetRow;
    for (i = 0; i < resamplePointCount; ++i)
    {
        assert (j >= 0 && j < pointCount - 1);
        target.refRow (i, targetRow);
        goDouble e = (t - accumLength[j]) / (accumLength[j+1] - accumLength[j]);
        targetRow =  row2 * e + row1 * (1-e);
        t += step;
        while (j < pointCount - 2 && t > accumLength[j+1])
        {
            ++j;
            source.refRow (j, row1);
            source.refRow (j + 1, row2);
        }
    }
    return true;
}
