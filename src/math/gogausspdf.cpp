#include <gogausspdf.h>
#include <gotypes.h>
#include <goerror.h>
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
            goError::print (this->getClassName(), "Variance is zero or lower than zero");
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
        goError::print (this->getClassName(), "operator() not defined for this input/output type combination.");
        assert (false);
        return (output_type)0;
    }
    
//    template <class input_type, class output_type>
    goDouble
    goGaussPDF<goDouble, goDouble>::operator () (const goDouble& input)
    {
        return myNormFactor * exp (-(input - myMean) * (input - myMean) * myVarianceReciprocal2);
    }
};

template class goMath::goGaussPDF <goDouble, goDouble>;
