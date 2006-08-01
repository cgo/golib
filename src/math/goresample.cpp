#include <goresample.h>
#include <golog.h>

template <class T>
bool goResampleLinear (const goMatrix<T>& source, goMatrix<T>& target, goSize_t resamplePointCount)
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
        goSize_t i;
        goVector<T> row1;
        goVector<T> row2;
        for (i = 0; i < pointCount - 1; ++i)
        {
            source.refRow (i, row1);
            source.refRow (i + 1, row2);
            curveLength += (row1 - row2).abs();
            accumLength[i + 1] = curveLength;
        }
    }

    goDouble step = curveLength / (double)(resamplePointCount-1);

    target.resize (resamplePointCount, source.getColumns());

    goDouble t = 0.0f;
    goSize_t i = 0;
    goSize_t j = 0;
    goVector<T> row1;
    goVector<T> row2;
    source.refRow (0, row1);
    source.refRow (1, row2);
    goVector<T> targetRow;
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

template bool goResampleLinear <goFloat> (const goMatrix<goFloat>&, goMatrix<goFloat>&, goSize_t);
template bool goResampleLinear <goDouble> (const goMatrix<goDouble>&, goMatrix<goDouble>&, goSize_t);
