/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goresample.h>
#include <gomath.h>
#include <golog.h>

/**
 * \addtogroup math
 * @{
 */
/** 
 * @brief Piecewise cubic resampling of a curve represented by a configuration matrix.
 *
 * The source matrix contains a point in each row, so does the target matrix
 * after successful completion of the function.
 *
 * @param source Source points.
 * @param target Target points.
 * @param resamplePointCount Point count intended for target.
 * @param closed If true, the source points will be treated as a closed curve, i.e. the first and last points are connected.
 * 
 * @return True if successful, false otherwise.
 */
template <class T>
bool goMath::resampleCubic (const goMath::Matrix<T>& source, goMath::Matrix<T>& target, goSize_t resamplePointCount, bool closed, goFixedArray<goDouble>* accumLength_)
{
    goSize_t pointCount = source.getRows();

//    if (source.getColumns() != 2)
//    {
//        goLog::warning("goResampleCubic(): point dimension != 2. Bailing out.");
//        return false;
//    }

    if (pointCount < 2)
    {
        goLog::warning("resampleCubic(): point count is < 2.");
        return false;
    }
    if (resamplePointCount < 2)
    {
        return false;
    }
    goDouble curveLength = 0.0;

    goFixedArray<goDouble>* accumLength__ = 0;
    if (!accumLength_)
    {
        accumLength__ = new goFixedArray<goDouble> (closed ? pointCount + 1 : pointCount);
    }
    else
    {
        accumLength_->setSize (closed ? pointCount + 1 : pointCount);
    }
    goFixedArray<goDouble>& accumLength = (accumLength__) ? *accumLength__ : *accumLength_;

    /*
     * Approximate the curve length with $ |\dot{c}(0)| + |\dot{c}(1)| $.
     * Crude, but better than line segment lengths.
     */
    {
        accumLength[0] = 0.0;
        goSize_t i;
        goMath::Vector<T> pm1, p0, p1, p2;
        if (!closed)
        {
            goMath::CubicSplineND<T> spline; 
            goSize_t N = 10;
            goMath::Vector<T> time (N);
            time.fillRange (0.0, 1.0, T(N));
            goFloat dt = 1.0 / T(N-1);
            time *= dt;
            goMath::Vector<T> D;
            for (i = 0; i < pointCount - 1; ++i)
            {
                source.refRow (goMath::max<goIndex_t>((goIndex_t)i - 1, 0), pm1);
                source.refRow (i, p0);
                source.refRow (goMath::min<goIndex_t>(i + 1, pointCount - 1), p1);
                source.refRow (goMath::min<goIndex_t>(i + 2, pointCount - 1), p2);
                spline.fit (pm1, p0, p1, p2);
                for (goSize_t j = 0; j < /* 9 */ N - 1; ++j)
                {
                    // curveLength += ((*spline(time[j])) - (*spline(time[j+1]))).norm2();
                    // curveLength += ::fabs (*spline.D(time[j]));
                    spline.D (time[j], D);
                    curveLength  += D.norm2() * dt;
                }
                // curveLength += ((p1 - pm1).norm2() * 0.5 + (p2 - p0).norm2() * 0.5) * 0.5;
                accumLength[i + 1] = curveLength;
            }
        }
        else
        {
            goMath::CubicSplineND<T> spline; 
            goSize_t N = 10;
            goMath::Vector<T> time (N);
            time.fillRange (0.0, 1.0, T(N));
            goFloat dt = 1.0 / T(N-1);
            time *= dt;
            goMath::Vector<T> D;
            for (goIndex_t i = 0; i < goIndex_t(pointCount); ++i)
            {
                if (i < 1)
                {
                    source.refRow (pointCount - 1, pm1);
                }
                else
                {
                    source.refRow (i - 1, pm1);
                }
                source.refRow (i, p0);
                if ((goSize_t)i >= pointCount - 1)
                {
                    source.refRow (i - pointCount + 1, p1);
                }
                else
                {
                    source.refRow (i + 1, p1);
                }
                if ((goSize_t)i >= pointCount - 2)
                {
                    source.refRow (i - pointCount + 2, p2);
                }
                else
                {
                    source.refRow (i + 2, p2);
                }
                // spline.fit (pm1, p0, p1, p2);
                spline.fit (pm1, p0, p1, p2);
                for (goSize_t j = 0; j < /* N-1 */ N - 1; ++j)
                {
                    // curveLength += ((*spline(time[j])) - (*spline(time[j+1]))).norm2();
                    spline.D (time[j], D);
                    curveLength += D.norm2() * dt;
                }
                //curveLength += ((p1 - pm1).norm2() * 0.5 + (p2 - p0).norm2() * 0.5) * 0.5;
                accumLength[i + 1] = curveLength;
            }
        }
    }

    goDouble step = 1.0;
    if (closed)
    {
        step = curveLength / (double)(resamplePointCount);
    }
    else
    {
        step = curveLength / (double)(resamplePointCount-1);
    }

    target.resize (resamplePointCount, source.getColumns());  //= columns must be 2

    goDouble t = 0.0f;
    goSize_t i = 0;
    goIndex_t j = 0;
    goMath::Vector<T> pm1, p0, p1, p2;

    source.refRow(0, p0);
    source.refRow(1, p1);
    source.refRow(2, p2);
    if (closed)
    {
        source.refRow(pointCount - 1, pm1);
    }
    else
    {
        source.refRow(0, pm1);
    }

    goMath::CubicSplineND<T> spline (pm1, p0, p1, p2);
//    goMath::Vector<T> row1;
//    goMath::Vector<T> row2;
//    source.refRow (0, row1);
//    source.refRow (1, row2);
    goMath::Vector<T> targetRow;
    for (i = 0; i < resamplePointCount; ++i)
    {
        // assert (j >= 0 && (closed || ((goSize_t)j < pointCount - 1)));
        target.refRow (i, targetRow);
        goDouble e = (t - accumLength[j]) / (accumLength[j+1] - accumLength[j]);
        spline.eval (e, targetRow);
        // targetRow = *spline (e);
        // targetRow =  row2 * e + row1 * (1-e);
        t += step;
        if (!closed)
        {
            bool refit = false;
            while (goSize_t(j) < pointCount - 2 && t > accumLength[j+1])
            {
                ++j;
                refit = true;
                //source.refRow (j, row1);
                //source.refRow (j + 1, row2);
            }
            if (refit)
            {
                source.refRow (goMath::max(j-1,0), pm1);
                source.refRow (j, p0);
                source.refRow (goMath::min<goIndex_t>(pointCount-1,j+1), p1);
                source.refRow (goMath::min<goIndex_t>(pointCount-1,j+2), p2);
                spline.fit (pm1, p0, p1, p2);
            }
        }
        else
        {
            bool refit = false;
            while (goSize_t(j) < pointCount - 1 && t > accumLength[j+1]) 
            {
                ++j;
                refit = true;
            }
            if (refit)
            {
                if (j <= 0)
                    source.refRow (pointCount - 1 + j, pm1);
                else
                    source.refRow (j - 1, pm1);
                source.refRow (j, p0);
                if ((goSize_t)j >= pointCount - 1)
                    source.refRow (j - pointCount + 1, p1);
                else
                    source.refRow (j+1, p1);
                if ((goSize_t)j >= pointCount - 2)
                    source.refRow (j - pointCount + 2, p2);
                else
                    source.refRow (j + 2, p2);
                spline.fit (pm1, p0, p1, p2);
            }
        }

    }

    if (accumLength__)
    {
        delete accumLength__;
        accumLength__ = 0;
    }
    return true;
}

template <class T>
void goMath::resampleLinear (const goFixedArray<T>& f, goFixedArray<T>& ret)
{
    const goSize_t N = f.getSize();
    const goSize_t M = ret.getSize();
    goDouble delta_x = goDouble(N-1) / goDouble(M-1);
    goDouble x = 0.0;

    goSize_t x0 = 0, x1 = 1;
    goDouble t = 0.0;
    for (goSize_t i = 0; i < M; ++i, x += delta_x)
    {
        x0 = goSize_t(::floor(x));
        x1 = goMath::min(x0 + 1, N-1);
        
        t = x - float(x0);
        ret[i] = t * f[x1] + (1.0 - t) * f[x0];
    }
}

/** @} */

/** 
 * @brief Linear resampling of a curve represented by a configuration matrix.
 *
 * The source matrix contains a point in each row, so does the target matrix
 * after successful completion of the function.
 *
 * @param source Source points.
 * @param target Target points.
 * @param resamplePointCount Point count intended for target.
 * 
 * @return True if successful, false otherwise.
 */
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
        goSize_t i;
        goMath::Vector<T> row1;
        goMath::Vector<T> row2;
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
    goMath::Vector<T> row1;
    goMath::Vector<T> row2;
    source.refRow (0, row1);
    source.refRow (1, row2);
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

template bool goResampleLinear <goFloat> (const goMath::Matrix<goFloat>&, goMath::Matrix<goFloat>&, goSize_t);
template bool goResampleLinear <goDouble> (const goMath::Matrix<goDouble>&, goMath::Matrix<goDouble>&, goSize_t);

template bool goMath::resampleCubic <goFloat> (const goMath::Matrix<goFloat>&, goMath::Matrix<goFloat>&, goSize_t, bool, goFixedArray<goDouble>*);
template bool goMath::resampleCubic <goDouble> (const goMath::Matrix<goDouble>&, goMath::Matrix<goDouble>&, goSize_t, bool, goFixedArray<goDouble>*);
template void goMath::resampleLinear <goFloat> (const goFixedArray<goFloat>&, goFixedArray<goFloat>&);
template void goMath::resampleLinear <goDouble> (const goFixedArray<goDouble>&, goFixedArray<goDouble>&);
