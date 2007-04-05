#ifndef GOMESSAGEPASSING_H
#define GOMESSAGEPASSING_H

#ifndef GOFACTORGRAPH_H
# include <gofactorgraph.h>
#endif
#ifndef GOGRAPH_H
# include <gograph.h>
#endif
#ifndef GOLIST_H
# include <golist.h>
#endif

/**
 * \addtogroup gm
 * @{
 */

/** 
 * @brief Base class for message passing algorithms. 
 *
 * Sum-product and max-sum are implemented in the class templates goMaxSum and goSumProduct.
 *
 * This does also terminate for graphs with loops, but the result may be very bad.
 * Loopy belief propagation with scheduling other than "two-way" is not yet implemented; initialisation
 * of messages is also not yet implemented.
 *
 * \li \c T      Type for the variables (currently recommended to be integer, since [0,myValueCount-1] is used as values).
 * \li \c Tfloat Floating point type for the "message" vectors. Determines the accuracy. Set to goFloat or goDouble.
 */
template <class T, class Tfloat>
class goMessagePassing : public goGraphAlgorithm< goFGNode<T,Tfloat>, goFGEdge<T, Tfloat > >
{
    public:
        enum Direction
        {
            FORWARD,
            BACKWARD
        };

    public:
        virtual ~goMessagePassing () {};

        virtual bool run (goFGNode<T,Tfloat>* root, goFactorGraph<T,Tfloat>& fg) = 0;
        
        /** 
         * @brief Recursive sum.
         *
         * Sums f over all variables except fixed_index. Currently just uses the indices as input functions
         * for the function f(). If other values should be used, change this function accordingly.
         *
         * @note This function is not used in this class, but left for completeness.
         *
         * @param f Functor to sum over.
         * @param X Input vector for f.
         * @param i Current index into X to sum over (start the recursion with 0).
         * @param fixed_index Fixed index: do not iterate over this index into X (remains fixed).
         * 
         * @return The sum of f over all variables except fixed_index.
         */
        inline goDouble sum (goFunctorBase1 <Tfloat, const goVector<T>& >* f, goVector<T>& X, goSize_t i, goSize_t fixed_index)
        {
            if (i >= X.getSize())
            {
                return (*f)(X);
            }
            if (i == fixed_index)
            {
                return sum (f, X, i+1, fixed_index);
            }
            goDouble s = 0.0;
            for (goSize_t j = 0; j < this->myValueCount; ++j)
            {
                X[i] = T(j);
                s += sum (f, X, i+1, fixed_index);
            }
            return s;
        };


        void      setValueCount (goSize_t c) { this->myValueCount = c; };
        goSize_t  getValueCount () const { return this->myValueCount; };
        void      setDirection  (Direction d) { this->myDirection = d; };
        Direction getDirection  () const { return this->myDirection; };
       
    protected:
        goMessagePassing () 
            : goGraphAlgorithm< goFGNode<T,Tfloat>, goFGEdge<T, Tfloat > > (),
              myValueCount (3),
              myDirection (FORWARD)
        {};

    private:
        //= Variables must all have the same number of possible values,
        //= so that the message vectors have same lengths. myValueCount is this length.
        goSize_t  myValueCount;
        Direction myDirection;
};

/** @} */
#endif
