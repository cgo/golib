/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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
    template <class T> class AffineTransform
    {
        public:
            AffineTransform ();
            AffineTransform (const goMath::Matrix<T>& A, const goMath::Vector<T>& t);
            virtual ~AffineTransform ();

            void set (const goMath::Matrix<T>& A_, const goMath::Vector<T>& t_)
            {
                this->A = A_;
                this->t = t_;
            };

            goMath::Matrix<T>& getA () { return this->A; };
            goMath::Vector<T>& getT () { return this->t; };
            const goMath::Matrix<T>& getA () const { return this->A; };
            const goMath::Vector<T>& getT () const { return this->t; };

            void apply (goMath::Vector<T>& v);
            void apply (const goMath::Matrix<T>& confMatrix, goMath::Matrix<T>& ret);

        private:
            goMath::Matrix<T> A;
            goMath::Vector<T> t;
    };
};

#endif
