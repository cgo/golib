#ifndef GOGAUSSPDF_H
#define GOGAUSSPDF_H

#include <gopdf.h>

namespace goMath
{
    /*! \addtogroup math
     * @{
     */
    /*! \brief Gauss' probability density function.
     */
    template <class input_type, class output_type>
    class goGaussPDF : public goPDF<input_type, output_type>
    {
        public:
            goGaussPDF ();
            virtual ~goGaussPDF ();

            virtual output_type operator () (const input_type& input);
            void                setMean     (const input_type& mean);
            void                setVariance (const input_type& variance);
            
        private:
            input_type myMean;                  // mu
            input_type myVariance;              // sigma^2
            input_type myVarianceReciprocal2;   // 1 / (2 * sigma^2)
            goDouble   myNormFactor;            // 1 / sqrt (2 Pi sigma^2)
    };
    /*! @} */
};

#endif
