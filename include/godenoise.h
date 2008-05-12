#ifndef GODENOISE_H
#define GODENOISE_H

#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif

namespace goSignal
{
    class TVL1Private;

    /** @addtogroup math 
     * @{ 
     */
    /** 
     * @brief TV-regularised L1-norm approximation of an image after [1].
     *
     * \par References
     * [1] Chan, T. F. & Esedoglu, S.:
     *     Aspects of Total Variation Regularized $L^1$ Function Approximation.
     *     SIAM Journal on Applied Mathematics, 2005, 65, 1817-1837
     */
    class TVL1
    {
        public:
            TVL1 (goAutoPtr<goSignal3DBase<void> > f, goDouble lambda);
            virtual ~TVL1 ();

            void setLambda (goDouble l);
            goDouble getLambda () const;
            void setAutoTimeStep (bool t);

            goAutoPtr<goSignal3DBase<void> > getU ();

            bool evolve (goDouble dt);

        protected:
            TVL1 (const TVL1&);
            TVL1& operator= (const TVL1&);

        private:
            TVL1Private* myPrivate;
    };
    /**
     * @}
     */
};

#endif
