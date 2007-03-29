#ifndef GOTTEST_H
#define GOTTEST_H

#ifndef GOMATH_H
# include <gomath.h>
#endif

namespace goMath
{
    /** 
     * @brief t-Test.
     *
     * See NIST/SEMATECH handbook: http://www.itl.nist.gov/div898/handbook/eda/section3/eda353.htm
     *
     * Tests if Y1 and Y2 have the same mean, with the given significance.
     * Y1 and Y2 need not have the same size nor variance.
     *
     * @param Y1      Vector of values 1.
     * @param size1   Size of Y1.
     * @param Y2      Vector of values 2.
     * @param size2   Size of Y2.
     * @param significance  Significance of the t-test (critical point where the tail is cut off is at alpha = significance*0.5). 
     *                      Default: 0.05.
     * 
     * @return True if Y1 and Y2 have the same mean with the given significance, false otherwise.
     */
    template <class scalarT, class vectorT>
    bool tTest (const vectorT& Y1, goSize_t size1, const vectorT& Y2, goSize_t size2, goDouble significance = 0.05)
    {
        scalarT mean1 = goMath::mean<vectorT, scalarT> (Y1, size1);
        scalarT mean2 = goMath::mean<vectorT, scalarT> (Y2, size2);
        scalarT s1    = goMath::variance<vectorT, scalarT> (Y1, size1, mean1);
        scalarT s2    = goMath::variance<vectorT, scalarT> (Y2, size2, mean2);
        if (s1 == scalarT(0) && s2 == scalarT(0))
        {
            if (fabs(mean1 - mean2) != scalarT(0))
            {
                goLog::warning ("goMath::tTest(): both variances are zero.");
                return false;
            }
        }
        scalarT T = (mean1 - mean2) / sqrt (s1/(float)size1 + s2/(float)size2);
        
        goDouble temp1 = s1 / (float)size1;
        goDouble temp2 = s2 / (float)size2;
        goDouble v1 = temp1 + temp2;
        goDouble v = (v1 * v1) / (temp1*temp1 / ((float)size1 - 1.0f) + temp2*temp2 / ((float)size2 - 1.0f));

        //= Find t_{alpha/2,v}
        goDouble x = 0.0;
        goDouble step = 1.0;
        goDouble t_alpha = 0.0;
        goDouble s = significance * 0.5;
        goDouble D = 1.0;
        goDouble temp_t = 0.0;
        while (fabs(D) > 1e-4)
        {
            t_alpha = x;
            temp_t = goMath::studentT<scalarT> (x, (goSize_t)v);
            goDouble D2 = temp_t - s;
            if (D * D2 < 0.0)
            {
                step = -step * 0.5;
            }
            x = x + step;
            D = D2;
        }

        if (fabs(T) > fabs(t_alpha))
        {
            return false;
        }

        return true;
    };
};

#endif
