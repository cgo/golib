#ifndef GOAFFINETRANSFORM_H
#define GOAFFINETRANSFORM_H

#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#ifndef GOVECTOR_H
# include <govector.h>
#endif

namespace goMath
{
    template <class T> class affineTransform
    {
        public:
            affineTransform ();
            affineTransform (const goMatrix<T>& A, const goVector<T>& t);
            virtual ~affineTransform ();

            void set (const goMatrix<T>& A_, const goVector<T>& t_)
            {
                this->A = A_;
                this->t = t_;
            };

            goMatrix<T>& getA () { return this->A; };
            goVector<T>& getT () { return this->t; };
            const goMatrix<T>& getA () const { return this->A; };
            const goVector<T>& getT () const { return this->t; };

            void apply (goVector<T>& v);
            void apply (const goMatrix<T>& confMatrix, goMatrix<T>& ret);

        private:
            goMatrix<T> A;
            goVector<T> t;
    };
};

#endif
