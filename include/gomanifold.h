#ifndef GOMANIFOLD_H
#define GOMANIFOLD_H

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
     * The Karcher mean is calculated by iterating using the update step
     * \f$ y_{i+1} = \exp_{y_i} \left( \frac{1}{N} \sum_{j=1}^N \log_{y_i} x_j \right) \f$
     * until \c max_iterations has been reached or until
     * \f$ \langle t,t \rangle_{y_i} < \varepsilon \f$,
     * with \f$ t = \frac{1}{N} \sum_{j=1}^N \log_{y_i} x_j\f$.
     *
     * @param start     Start iterator, pointing to the first element x_j.
     * @param count     Number of elements N.
     * @param manifold  Manifold object (e.g. SO3).
     * @param meanRet   Return value, contains the mean after returning true.
     * @param max_iterations  Max. number of iterations. Default: 1000
     * @param epsilon   "Small" floating point value. Default: 1e-6
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
    class SO3 : public Manifold <goMath::Matrix<T>, goMath::Vector<T> >
    {
        public:
            typedef goMath::Matrix<T> Element;
            typedef goMath::Vector<T> Tangent;

            SO3 ();
            virtual ~SO3 ();
            virtual void     exp (const Element& e, const Tangent& v, Element& ret);
            virtual void     log (const Element& e1, const Element& e2, Tangent& ret);
            virtual goDouble innerProduct (const Element& e, const Tangent& v1, const Tangent& v2);
            void matrix (const goMath::Vector<T>& w, goMath::Matrix<T>& ret);
            void vector (const goMath::Matrix<T>& ret, goMath::Vector<T>& w);

        private:
            Element myId;
    };

    /** 
     * @brief Simple linear vector space.
     */
    template <class T>
    class LinearSpace : public Manifold <goMath::Vector<T>, goMath::Vector<T> >
    {
        public:
            typedef goMath::Vector<T> Element;
            typedef goMath::Vector<T> Tangent;

            LinearSpace ();
            virtual ~LinearSpace ();
            virtual void     exp (const Element& e, const Tangent& v, Element& ret);
            virtual void     log (const Element& e1, const Element& e2, Tangent& ret);
            virtual goDouble innerProduct (const Element& e, const Tangent& v1, const Tangent& v2);
    };

    /** 
     * @brief Unit sphere.
     */
    template <class T>
    class UnitSphere : public Manifold <goMath::Vector<T>, goMath::Vector<T> >
    {
        public:
            typedef goMath::Vector<T> Element;
            typedef goMath::Vector<T> Tangent;

            UnitSphere ();
            virtual ~UnitSphere ();
            virtual void     exp (const Element& e, const Tangent& v, Element& ret);
            virtual void     log (const Element& e1, const Element& e2, Tangent& ret);
            virtual goDouble innerProduct (const Element& e, const Tangent& v1, const Tangent& v2);
    };

    /** @example karchermean/km.cpp
     * Example for Karcher mean calculation using goMath::karcherMean()
     */
    /** @example so3/so3.cpp
     * Example for using the rotation group manifold SO3.
     */
    /** @} */
};

#endif
