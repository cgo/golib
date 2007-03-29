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
        goIndex_t i;
        goVector<T> row1;
        goVector<T> row2;
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
    goVector<T> row1;
    goVector<T> row2;
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
