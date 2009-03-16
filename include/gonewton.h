#ifndef GONEWTON_H
#define GONEWTON_H

#ifndef GOMATH_H
# include <gomath.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goMath
{
    // template <class callable_, class type_> class NewtonOptPrivate;

    template <class callable_, class type_>
    class NewtonOpt
    {
        public:
            typedef goMath::Vector<type_> vector_type;
            typedef goMath::Matrix<type_> matrix_type;

        public:
            NewtonOpt (double eps = 0.01) 
                : myEpsilon (eps)
            { }
            virtual ~NewtonOpt () { }

            //= Yes, I know that virtuals are slower.
            //= x is modified during the call, but the same when the call returns.
            virtual void grad (vector_type& x, vector_type& ret)
            {
                const goSize_t sz = x.getSize ();
                if (ret.getSize () != sz)
                {
                    ret.setSize (sz);  //= setSize is faster than resize.
                }

                double factor = 1.0 / myEpsilon;
                type_ temp = type_ (0);
                type_ temp2 = type_ (0);
                type_ fx = myF (x);

                for (goSize_t i = 0; i < sz; ++i)
                {
                    temp = x[i];
                    x[i] += myEpsilon;
                    temp2 = myF (x);
                    x[i] = temp;       // Restore x[i]
                    ret[i] = (temp2 - fx) * factor;
                }
            }

            virtual void hessian (vector_type& x, matrix_type& ret)
            {
                // H_{i,j} = d^2f / (dx_i dx_j)
                const goSize_t sz = x.getSize ();
                if (ret.getColumns () != sz || ret.getRows () != sz)
                {
                    ret.resize (sz, sz);  //= setSize is faster than resize.
                }

                //= precompute f(x_1,...,x_i + epsilon,...,x_n)
                vector_type f_xi (sz);
                type_ temp = type_ (0);

                for (goSize_t i = 0; i < sz; ++i)
                {
                    temp = x[i];
                    x[i] += myEpsilon;
                    f_xi[i] = myF (x);
                    x[i] = temp;
                }

                type_ fx = myF (x);

                double factor = 1.0 / (myEpsilon * myEpsilon);

                for (goSize_t i = 0; i < sz; ++i)
                {
                    temp = x[i];
                    x[i] += myEpsilon;
                    type_ temp2 = type_ (0);

                    for (goSize_t j = i; j < sz; ++j)
                    {
                        temp2 = x[j];
                        x[j] += myEpsilon;
                        ret (i,j) = ((myF (x) - f_xi[j]) - (f_xi[i] - fx)) * factor;
                        x[j] = temp2; // Restore x[j]
                        ret (j,i) = ret (i,j);
                    }
                    x[i] = temp;  // Restore x[i]
                }
            }

        private:
            // NewtonOptPrivate<callable_, type_> *myPrivate;
            double myEpsilon;
            callable_ myF;
    };
};

#endif
