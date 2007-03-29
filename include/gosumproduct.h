#ifndef GOSUMPRODUCT_H
#define GOSUMPRODUCT_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif
#ifndef GOGRAPH_H
# include <gograph.h>
#endif
#ifndef GOFUNCTOR_H
# include <gofunctor.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#ifndef GOGRAPH_H
# include <gograph.h>
#endif
#ifndef GOFACTORGRAPH_H
# include <gofactorgraph.h>
#endif

/** 
 * \addtogroup gm
 * @{
 */

/** 
 * @brief The sum-product algorithm.
 * @verbatim
    Bishop, C.M., Pattern Recognition and Machine Learning, Springer, 2006, chapter 8
   @endverbatim
 * @author Christian Gosch
 */
template <class T, class Tfloat>
class goSumProduct : public goObjectBase
{
    public:
        goSumProduct ();
        virtual ~goSumProduct ();

        /** 
         * @brief Run the sum-product algorithm.
         * 
         * @param fg Factor graph to run the algorithm on.
         * @param valueCount The values of the variables are in the range [0,valueCount-1].
         * 
         * @return True if successful, false otherwise.
         */
        virtual bool run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);

        bool marginal (
                goFGNodeVariable<T,Tfloat>* variable, 
                goSize_t                    valueCount, 
                goVector<Tfloat>&           marginalRet);
        
        Tfloat norm (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);
};

/** 
 * @brief The max-sum algorithm.
 * @verbatim
    Bishop, C.M., Pattern Recognition and Machine Learning, Springer, 2006, chapter 8
   @endverbatim
 * @note The factors in the factor graph must return logarithms, no extra log() is called.
 * @author Christian Gosch
 */
template <class T, class Tfloat>
class goMaxSum : public goObjectBase
{
    public:
        goMaxSum ();
        virtual ~goMaxSum ();

        /** 
         * @brief Run the max-sum algorithm.
         * 
         * @param fg Factor graph to run the algorithm on.
         * @param valueCount The values of the variables are in the range [0,valueCount-1].
         * 
         * @return True if successful, false otherwise.
         */
        virtual bool run (goFactorGraph<T,Tfloat>& fg, goSize_t valueCount);
};

/** @} */

#endif
