#include <gogausspdf.h>
#include <gotypes.h>
#include <golog.h>
#include <gostring.h>
#include <gorandom.h>
#include <assert.h>
#include <math.h>

namespace goMath
{
    template <class input_type, class output_type>
    goGaussPDF<input_type, output_type>::goGaussPDF ()
        : goPDF<input_type, output_type> (),
          myMean                ((input_type)0.0),
          myVariance            ((input_type)1.0),
          myVarianceReciprocal2 ((input_type)1.0)
    {
        this->setVariance ((input_type)1.0);
        this->setClassID(GO_GAUSSPDF);
    }

    template <class input_type, class output_type>
    goGaussPDF<input_type, output_type>::~goGaussPDF ()
    {
    }

    template <class input_type, class output_type>
    void
    goGaussPDF<input_type, output_type>::setMean (const input_type& mean)
    {
        myMean = mean;
    }
    
    template <class input_type, class output_type>
    void
    goGaussPDF<input_type, output_type>::setVariance (const input_type& variance)
    {
        if (variance <= 0.0f)
        {
            goLog::warning ("goGaussPDF::setVariance(): Variance is zero or lower than zero");
            myVariance            = 0.0;
            myVarianceReciprocal2 = 1.0;
            myNormFactor          = 1.0;
        }
        else
        {
            assert (variance > 0.0);
            myVariance            = variance;
            myVarianceReciprocal2 = 1.0 / (2.0 * variance);
            myNormFactor          = 1.0 / sqrt (2 * M_PI * variance);
        }
    }
    
    template <class input_type, class output_type>
    output_type
    goGaussPDF<input_type, output_type>::operator () (const input_type& input) const
    {
        //goLog::warning ("goGaussPDF::operator() not defined for this input/output type combination.");
        //assert (false);
        //return (output_type)1;
        return myNormFactor * exp (-(input - myMean) * (input - myMean) * myVarianceReciprocal2);
    }
    
    //===================================================================
    template <class input_vector, class scalar_type>
    goMultiGaussPDF<input_vector, scalar_type>::goMultiGaussPDF ()
        : goPDF<input_vector, scalar_type> (),
          myMean          (),
          myCovariance    (),
          myCovarianceInv (),
          myNormFactor    (1.0),
          sum_xxT         (1,1),
          N               (0.0f)
    {
    };

    template <class input_vector, class scalar_type>
    goMultiGaussPDF<input_vector, scalar_type>::goMultiGaussPDF (const input_vector& mean, 
                                                                 const goMatrix<scalar_type>& cov, 
                                                                 scalar_type normFactor)
        : goPDF<input_vector, scalar_type> (),
          myMean          (mean),
          myCovarianceInv (cov),
          myNormFactor    (normFactor)
    {
        if (!myCovarianceInv.invert())
        {
            goLog::warning ("goMultiGaussPDF::goMultiGaussPDF(): could not invert covariance matrix.", this);
        }
    };

    template<class input_vector, class scalar_type>
    goMultiGaussPDF<input_vector, scalar_type>::~goMultiGaussPDF ()
    {
    };

    template<class input_vector, class scalar_type>
    scalar_type goMultiGaussPDF<input_vector, scalar_type>::operator() (const input_vector& input) const
    {
//        if (myNormFactor == 0.0f)
//        {
//            //= Graceful handling of special case with no variation at all.
//            if (input == myMean)
//                return 1.0f;
//            else
//                return 0.0f;
//        }
        input_vector temp = input - myMean;
        scalar_type f = -0.5 * (temp * (myCovarianceInv * temp));
        return myNormFactor * exp(f);
    }
    
    /** 
     * @brief Resets sample count, covariance matrix, etc., before using update() to learn a 
     * distribution from samples.
     * 
     * @param N Vector length of samples. You may leave this open, the first
     * update() call will set the sizes then.
     */
    template<class input_vector, class scalar_type>
        void goMultiGaussPDF<input_vector, scalar_type>::reset (goSize_t N)
        {
            this->myMean.resize (N);
            this->myMean.fill (scalar_type(0));
            this->myCovariance.resize (N,N);
            // this->myCovariance.fill (scalar_type(0));
            this->myCovariance.setIdentity ();
            this->myNormFactor = 1.0f;

            this->N = 0.0f;
            this->sum_xxT.resize (N,N);
            this->sum_xxT.fill (scalar_type(0));
        }
        
    /** 
     * @brief Update current mean and covariance.
     * 
     * You have to call reset() before starting to learn a distribution with this function.
     * When you are done, you must call updateFlush() to calculate the inverse covariance and the like.
     * 
     * @param input Sample vector.
     */
    template<class input_vector, class scalar_type>
        void goMultiGaussPDF<input_vector, scalar_type>::update (const input_vector& input)
        {
            goFloat factor = 1.0f / (this->N + 1.0f);
            this->sum_xxT *= this->N * factor;
            goVectorOuter<scalar_type> (scalar_type(factor), input, input, this->sum_xxT);

            this->myMean *= this->N * factor;
            goVectorAdd<scalar_type> (scalar_type(factor), input, this->myMean);
            
            if (this->N >= 2)
            {
                this->myCovariance = this->sum_xxT;
                factor = -1.0f; 
                goVectorOuter<scalar_type> (scalar_type(factor), this->myMean, this->myMean, this->myCovariance);
            }
            this->N += 1.0f;
        }

    /** 
     * @brief Ending learning from samples.
     * 
     * You must call this function when you are done learning a distribution from samples with update().
     * Note that you can always call update() after updateFlush(), but in order to use the new, updated
     * distribution, you must call updateFlush() before use.
     */
    template<class input_vector, class scalar_type>
        void goMultiGaussPDF<input_vector, scalar_type>::updateFlush ()
        {
            //= Also sets inverse covariance.
            this->setCovariance (this->myCovariance);
        }
    
//    template<class input_vector, class scalar_type>
//    bool goMultiGaussPDF<input_vector, scalar_type>::fromSamples (const input_vector* vectors, goIndex_t count)
//    {
//        assert (vectors);
//        if (count < 1 || !vectors)
//            return false;
//        
//        myMean.resize (vectors[0].getSize());
//        goIndex_t i;
//        for (i = 0; i < count; ++i)
//        {
//            
//        }
//    }

    template <class input_vector, class scalar_type>
        void goMultiGaussPDF<input_vector,scalar_type>::setCovariance (const goMatrix<scalar_type>& cov)
        {
            myCovariance = cov;
            myCovarianceInv = cov;
            if (!myCovarianceInv.invert())
            {
                goLog::warning ("goMultiGaussPDF::setCovariance(): could not invert covariance matrix. Setting it to identity.", this);
                myCovariance.setIdentity();
                myCovarianceInv = myCovariance;
            }
            //= Normalisation factor is currently only set for the special case of univariate Gaussian. FIXME
            if (cov.getRows() == 1 && cov.getColumns() == 1)
            {
                if (cov(0,0) <= 0.0)
                {
                    goDouble e = 0.0;
                    while (fabs(e) < 1e-4)
                        e = goRandom ();
                    this->myMean[0] += e;
                    myCovariance(0,0) = e*e; //= Set to _some_ value, here a random disturbance. FIXME
                    myCovarianceInv(0,0) = 1.0 / myCovariance(0,0);
                }
                this->myNormFactor = 1.0 / sqrt (2.0 * M_PI * myCovariance(0,0));
            }
        }

    template <class input_vector, class scalar_type>
        const goMatrix<scalar_type>& goMultiGaussPDF<input_vector,scalar_type>::getCovarianceInv () const
        {
            return this->myCovarianceInv;
        }
    template <class input_vector, class scalar_type>
        const goMatrix<scalar_type>& goMultiGaussPDF<input_vector,scalar_type>::getCovariance () const
        {
            return this->myCovariance;
        }
    
    template <class input_vector, class scalar_type>
        void goMultiGaussPDF<input_vector,scalar_type>::setMean (const input_vector& m)
        {
            this->myMean = m;
        }

    template <class input_vector, class scalar_type>
        const input_vector& goMultiGaussPDF<input_vector,scalar_type>::getMean () const
        {
            return this->myMean;
        }
};

template class goMath::goGaussPDF <goDouble, goDouble>;
template class goMath::goGaussPDF <goFloat, goFloat>;
template class goMath::goMultiGaussPDF <goVectorf, goFloat>;
template class goMath::goMultiGaussPDF <goVectord, goDouble>;
// template class goMath::goMultiGaussPDF <goVectord, goDouble>;
