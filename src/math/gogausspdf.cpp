#include <gogausspdf.h>
#include <gotypes.h>
#include <golog.h>
#include <gostring.h>
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
        this->setClassName ("goGaussPDF");
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
        if (variance <= 0.0)
        {
            goLog::warning ("goGaussPDF::setVariance(): Variance is zero or lower than zero");
            return;
        }
        assert (variance > 0.0);

        myVariance            = variance;
        myVarianceReciprocal2 = 1.0 / (2.0 * variance);
        myNormFactor          = 1.0 / sqrt (2 * M_PI * variance);
    }
    
    template <class input_type, class output_type>
    output_type
    goGaussPDF<input_type, output_type>::operator () (const input_type& input)
    {
        goLog::warning ("goGaussPDF::operator() not defined for this input/output type combination.");
        assert (false);
        return (output_type)0;
    }
    
//    template <class input_type, class output_type>
template<> goDouble goGaussPDF<goDouble, goDouble>::operator () (const goDouble& input)
    {
        return myNormFactor * exp (-(input - myMean) * (input - myMean) * myVarianceReciprocal2);
    }

    //===================================================================
    template <class input_vector, class output_type>
    goMultiGaussPDF<input_vector, output_type>::goMultiGaussPDF ()
        : goPDF<input_vector, output_type> (),
          myMean          (),
          myCovarianceInv (),
          myNormFactor    (0.0)
    {
    };
    template<class input_vector, class output_type>
    goMultiGaussPDF<input_vector, output_type>::~goMultiGaussPDF ()
    {
    };

    template<class input_vector, class output_type>
    output_type goMultiGaussPDF<input_vector, output_type>::operator() (const input_vector& input)
    {
        input_vector temp = input - myMean;
        input_vector temp_t = temp;
        temp_t.transpose();
        goDouble f = -0.5 * temp_t * myCovarianceInv * temp;
        return myNormFactor * exp(f);
    }
    
    template<class input_vector, class output_type>
    bool goMultiGaussPDF<input_vector, output_type>::fromSamples (const input_vector* vectors, goIndex_t count)
    {
        assert (vectors);
        if (count < 1 || !vectors)
            return false;
        
        myMean.resize (vectors[0].getSize());
        goIndex_t i;
        for (i = 0; i < count; ++i)
        {
            
        }
    }
   
};

template class goMath::goGaussPDF <goDouble, goDouble>;
