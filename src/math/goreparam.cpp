#include <goreparam.h>
#include <gomath.h>

namespace goMath
{
    template <class T> class ReparamPrivate
    {
        public:
            ReparamPrivate ()
            {
            };

            ~ReparamPrivate ()
            {
            };
    };

    template <class T> Reparam<T>::Reparam ()
    {
    }

    template <class T> Reparam<T>::~Reparam ()
    {
    }

    /** 
     * @brief Reparametrise close to equidistant points.
     * 
     * @param curve    Configuration matrix with curve points in the rows.
     * @param maxIter  Max. iterations. Stops after this many steps.
     * @param closed   If true, curve is considered closed (default: true)
     * @return True if successful, false otherwise.
     */
    template <class T> bool Reparam<T>::equidistant (goMatrix<T>& curve, goSize_t maxIter, bool closed)
    {
        goVector<T> fi (0);
        goVector<T> pi (0);
        goVector<T> pip1 (0);
        goVector<T> pim1 (0);
        goVector<T> t (2);

        const int N = static_cast<int> (curve.getRows ());
        goSize_t iter = 0;
        if (closed)
        {
            while (iter < maxIter)
            {
                for (int i = 0; i < N; ++i)
                {
                    curve.refRow (i, pi);
                    curve.refRow (goMath::mod (i + 1, N), pip1);
                    curve.refRow (goMath::mod (i - 1, N), pim1);
                    t = pip1;
                    t -= pim1;
                    t *= 1.0 / t.norm2 ();
                    pi += t * (((pip1 + pim1) * 0.5 - pi) * t);
                }

                ++iter;
            }
        }
        else
        {
            while (iter < maxIter)
            {
                for (goSize_t i = 1; i < N - 1; ++i)
                {
                    curve.refRow (i, pi);
                    curve.refRow (i + 1, pip1);
                    curve.refRow (i - 1, pim1);
                    t = pip1;
                    t -= pim1;
                    t *= 1.0 / t.norm2 ();
                    pi += t * (((pip1 - pim1) * 0.5 - pi) * t);
                }

                ++iter;
            }
        }

        return true;
    }
};

template class goMath::Reparam <goFloat>;
template class goMath::Reparam <goDouble>;
