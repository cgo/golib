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
