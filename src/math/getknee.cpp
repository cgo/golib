#include <gomath.h>
#include <goplot.h>
#include <govector.h>

/**
 * @brief Find the "knee" point of a curve (typically eigenvalues).
 *
 * Fits all possibilities of 2 lines to the curve by
 * dividing the data points at a point N1 and fitting one line to the left, one to the right.
 * The N1 that results in the smallest total error of the fit will be returned as
 * "knee" point of the curve given by x and y input vectors.
 * 
 * @param x X coordinates of the input curve.
 * @param y Y coordinates of the input curve.
 * 
 * @return X coordinate index at which the knee point is estimated.
 */
template <class T>
goSize_t goMath::getKnee (const goFixedArray<T>& x, const goFixedArray<T>& y)
{
    if (x.getSize() <= 2)
    {
        return x.getSize() == 0 ? 0 : 1;
    }
    assert (x.getSize() == y.getSize());
    //= Fit all possible line pairs y_k = alpha_k * x + beta_k to the data
    goArray<goDouble> alpha0 (y.getSize());
    goArray<goDouble> beta0  (y.getSize());
    goArray<goDouble> alpha1 (y.getSize());
    goArray<goDouble> beta1  (y.getSize());
    goArray<goDouble> RMSE  (y.getSize());
    
    goSize_t N = y.getSize();
    goSize_t N1 = y.getSize() - 2;

    T sumX   [2];
    T sumY   [2];
    T sumX_2 [2];
    T sumXY  [2];

#define CALC_ALPHA(__i__,N_rec) \
    (sumXY[__i__] - N_rec * sumX[__i__] * sumY[__i__]) / (sumX_2[__i__] - N_rec * sumX[__i__] * sumX[__i__]);

#define CALC_BETA(__i__,__alpha__,N_rec) \
    (sumY[__i__] - __alpha__ * sumX[__i__]) * N_rec;

    //= Initialise sums.
    goSize_t i;
    sumX[0]   = T(0.0);
    sumY[0]   = T(0.0);
    sumX_2[0] = T(0.0);
    sumXY[0]  = T(0.0);
    sumX[1]   = T(0.0);
    sumY[1]   = T(0.0);
    sumX_2[1] = T(0.0);
    sumXY[1]  = T(0.0);
    for (i = 0; i < N1; ++i)
    {
        sumX[0]   += x[i];
        sumY[0]   += y[i];
        sumXY[0]  += x[i] * y[i];
        sumX_2[0] += x[i] * x[i];
    }
    for (i = N1; i < N; ++i)
    {
        sumX[1]   += x[i];
        sumY[1]   += y[i];
        sumXY[1]  += x[i] * y[i];
        sumX_2[1] += x[i] * x[i];
    }
    
    T NL_rec = 1.0 / static_cast<goDouble>(N1);
    T NR_rec = 1.0 / static_cast<goDouble>(N - N1);
    alpha0[N1] = CALC_ALPHA(0,NL_rec);
    beta0[N1]  = CALC_BETA(0,alpha0[N1],NL_rec);
    alpha1[N1] = CALC_ALPHA(1,NR_rec);
    beta1[N1]  = CALC_BETA(1,alpha1[N1],NR_rec);

    //= Calculate squared error of the current lines.
    {
        goSize_t i;
        T RMSEL = T(0);
        T RMSER = T(0);
        for (i = 0; i < N1; ++i)
        {
            T temp = alpha0[N1] * x[i] + beta0[N1] - y[i];
            temp *= temp;
            RMSEL += temp;
        }
        RMSEL /= (float)N1;
        for (i = N1; i < N; ++i)
        {
            T temp = alpha1[N1] * x[i] + beta1[N1] - y[i];
            temp *= temp;
            RMSER += temp;
        }
        RMSER /= (float)(N - N1);
        RMSE[N1] = RMSEL / (NL_rec * N) + RMSER / (NR_rec * N);
        // RMSE[N1] = RMSEL + RMSER;
    }

    for (N1 = N - 3; N1 > 1; --N1)
    {
        //= Update the sums for calculating the least squares lines.
        sumX[0]   -= x[N1];
        sumY[0]   -= y[N1];
        sumXY[0]  -= x[N1] * y[N1];
        sumX_2[0] -= x[N1] * x[N1];
        sumX[1]   += x[N1];
        sumY[1]   += y[N1];
        sumXY[1]  += x[N1] * y[N1];
        sumX_2[1] += x[N1] * x[N1];
        //= Calculate 1/N_right and 1/N_left
        NL_rec = 1.0 / static_cast<goDouble>(N1);
        NR_rec = 1.0 / static_cast<goDouble>(N - N1);
        //= Fit left and right lines y = alpha[0|1] * x + beta[0|1]
        alpha0[N1] = CALC_ALPHA(0,NL_rec);
        beta0[N1]  = CALC_BETA(0,alpha0[N1],NL_rec);
        alpha1[N1] = CALC_ALPHA(1,NR_rec);
        beta1[N1]  = CALC_BETA(1,alpha1[N1],NR_rec);
        //= Calculate squared error of the current lines.
        {
            goSize_t i;
            T RMSEL = T(0);
            T RMSER = T(0);
            for (i = 0; i < N1; ++i)
            {
                T temp = alpha0[N1] * x[i] + beta0[N1] - y[i];
                temp *= temp;
                RMSEL += temp;
            }
            RMSEL /= (float)N1;
            for (i = N1; i < N; ++i)
            {
                T temp = alpha1[N1] * x[i] + beta1[N1] - y[i];
                temp *= temp;
                RMSER += temp;
            }
            RMSER /= (float)(N - N1);
            RMSE[N1] = RMSEL / (NL_rec * N) + RMSER / (NR_rec * N);
            // RMSE[N1] = RMSEL + RMSER;
        }
    }

    // 2 ... N - 2
    goVectord temp (N-3);
    goVectord temp2 (N-3);
    T minError = RMSE[2];
    goSize_t minIndex = 2;
    for (i = 2; i < N-1; ++i)
    {
        temp[i-2] = RMSE[i];
        temp2[i-2] = i;
        if (RMSE[i] < minError)
        {
            minError = RMSE[i];
            minIndex = i;
        }
    }

    //= Outputs for checking.
#if 0
    printf ("minIndex == %d\n", minIndex);
    {
        goPlotter plotter;
        plotter.addCurve (temp2, temp, "Square error");
        plotter.plotPostscript ("2lines_knee_finding_square_error.ps");
    }
    {
        goPlotter plotter;
        goVectord tempX (minIndex + 1);
        goVectord tempY (tempX.getSize());
        for (i = 0; i < minIndex + 1; ++i)
        {
            tempX[i] = (goDouble)i;
            tempY[i] = alpha0[minIndex] * tempX[i] + beta0[minIndex];
        }
        plotter.addCurve (tempX, tempY, "Line 1");
        tempX.setSize (x.getSize() - minIndex + 1);
        tempY.setSize (tempX.getSize());
        for (i = minIndex - 1; i < x.getSize(); ++i)
        {
            tempX[i-minIndex + 1] = (goDouble)(i);
            tempY[i-minIndex + 1] = alpha1[minIndex] * tempX[i-minIndex + 1] + beta1[minIndex];
        }
        plotter.addCurve (tempX, tempY, "Line 2");
        plotter.addCurve (x, y, "Eigenvalue plot of an affinity matrix", "");
        plotter.plotPostscript (goString("2lines_knee_finding.ps"));
    }
#endif
    
    return minIndex;
#undef CALC_ALPHA
#undef CALC_BETA
}

// template goSize_t goMath::getKnee<goFloat> (const goFixedArray<goFloat>& x, const goFixedArray<goFloat>& y);
template goSize_t goMath::getKnee<goDouble> (const goFixedArray<goDouble>& x, const goFixedArray<goDouble>& y);
