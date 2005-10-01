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

    template <class input_vector, class output_type>
    class goMultiGaussPDF : public goPDF<input_vector, output_type>
    {
        public:
            goMultiGaussPDF ();
            virtual ~goMultiGaussPDF ();
            virtual output_type operator()    (const input_vector& input);
            bool                fromSamples   (const input_vector* vectors, goIndex_t count);
            goMatrixd&          getCovariance ();
            const goMatrixd&    getCovariance () const;
            input_vector&       getMean       ();
            const input_vector& getMean       () const;

        private:
            input_vector myMean;
            goMatrixd    myCovarianceInv;
            goDouble     myNormFactor;
    };
    /*! @} */
};

#endif
