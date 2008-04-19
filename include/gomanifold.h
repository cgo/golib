namespace goMath
{
    /**
     * \addtogroup math
     * @{
     */
    /** 
     * @brief Interface for manifolds.
     *
     * Derived classes must provide for
     * exponential and logarithmic map and inner product.
     * The default distance d() is 
     * \f$ \sqrt{\langle v,v \rangle_{e_1}} \f$
     * with \f$ v = Log_{e_1}(e_2) \f$.
     */
    template <class element_type, class tangent_type>
    class Manifold
    {
        public:
            typedef element_type Element;
            typedef tangent_type Tangent;

            virtual ~Manifold ();
            
            virtual void     exp (const Element& e, const Tangent& v, Element& ret) = 0;
            virtual void     log (const Element& e1, const Element& e2, Tangent& ret) = 0;
            virtual goDouble innerProduct (const Element& e, const Tangent& v1, const Tangent& v2) = 0;
            virtual goDouble d   (const Element& e1, const Element& e2);

        protected:
            Manifold ();
    };

    /** 
     * @brief Karcher mean for generic Riemannian manifolds.
     * 
     * @param start     Start iterator.
     * @param count     Number of elements.
     * @param manifold  Manifold object.
     * @param meanRet   Return value, contains the mean after returning true.
     * @param max_iterations  Max. number of iterations.
     * @param epsilon   "Small" floating point value.
     * 
     * @return True if mean could be found within max_iterations, false otherwise.
     */
    template <class iterator_type, class manifold_type>
        bool karcherMean (iterator_type           start, 
                          int                     count, 
                          manifold_type&          manifold, 
                          typename manifold_type::Element& meanRet, 
                          int                     max_iterations = 1000, 
                          double                  epsilon = 1e-6)
        {
            typename manifold_type::Element mean = *start;
            meanRet = mean;
            typename manifold_type::Tangent sum, temp;

            float f = 1.0f / static_cast<float> (count);
            iterator_type it;
            int iterations = 0;
            while (iterations < max_iterations)
            {
                it = start;
                ++it;
                manifold.log (mean, *start, sum);
                for (int i = 1; i < count; ++i, ++it)
                {
                    manifold.log (mean, *it, temp);
                    sum += temp;
                }
                sum *= f;
                manifold.exp (mean, sum, meanRet);
                printf ("Inner product: %f\n", manifold.innerProduct (mean, sum, sum));
                if (manifold.innerProduct (mean, sum, sum) < epsilon)
                    return true;
                mean = meanRet;
                ++iterations;
            }

            return false;
        };

    /** 
     * @brief Rotation group.
     */
    template <class T>
    class SO3 : public Manifold <goMatrix<T>, goVector<T> >
    {
        public:
            typedef goMatrix<T> Element;
            typedef goVector<T> Tangent;

            SO3 ();
            virtual ~SO3 ();
            virtual void     exp (const Element& e, const Tangent& v, Element& ret);
            virtual void     log (const Element& e1, const Element& e2, Tangent& ret);
            virtual goDouble innerProduct (const Element& e, const Tangent& v1, const Tangent& v2);
            void matrix (const goVector<T>& w, goMatrix<T>& ret);
            void vector (const goMatrix<T>& ret, goVector<T>& w);

        private:
            Element myId;
    };
    /** @} */
};
