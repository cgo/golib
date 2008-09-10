#ifndef GOGAUSSPDF_H
#define GOGAUSSPDF_H

#include <gopdf.h>
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif

namespace goMath
{
    /*! \addtogroup math
     * @{
     */
    /*! \brief Gauss' probability density function.
     */
    template <class input_type, class output_type>
    class GaussPDF : public PDF<input_type, output_type>
    {
        public:
            GaussPDF ();
            virtual ~GaussPDF ();

            virtual output_type operator () (const input_type& input) const;
            void                setMean     (const input_type& mean);
            void                setVariance (const input_type& variance);
            
        private:
            input_type myMean;                  // mu
            input_type myVariance;              // sigma^2
            input_type myVarianceReciprocal2;   // 1 / (2 * sigma^2)
            goDouble   myNormFactor;            // 1 / sqrt (2 Pi sigma^2)
    };

    /** 
     * @brief Vector values Gauss distribution.
     *
     * You can use reset(), update(), and updateFlush() to learn a distribution from samples.
     * 
     * See the latex documentation in doc/gogausspdf for details.
     * 
     * @todo Normalisation factor.
     */
    template <class input_vector, class scalar_type>
    class MultiGaussPDF : public PDF<input_vector, scalar_type>
    {
        public:
            MultiGaussPDF ();
            MultiGaussPDF (const input_vector& mean, const goMath::Matrix<scalar_type>& cov, scalar_type normFactor = scalar_type(1));

            //= The standard operator= and copy constructor will do.

            virtual ~MultiGaussPDF ();
            virtual scalar_type           operator()    (const input_vector& input) const;

            void                          reset         (goSize_t N = 1);
            void                          update        (const input_vector& v);
            void                          updateFlush   ();
            
            // bool                          fromSamples   (const input_vector* vectors, goIndex_t count);
            void                          setCovariance (const goMath::Matrix<scalar_type>& cov);
            const goMath::Matrix<scalar_type>&  getCovarianceInv () const;
            const goMath::Matrix<scalar_type>&  getCovariance () const;
            void                          setMean       (const input_vector& m);
            const input_vector&           getMean       () const;

        private:
            input_vector          myMean;
            goMath::Matrix<scalar_type> myCovariance;
            goMath::Matrix<scalar_type> myCovarianceInv;
            scalar_type           myNormFactor;

            //= For learning
            goMath::Matrix<scalar_type> sum_xxT;
            goFloat               N;
    };
    /*! @} */
};

#endif
