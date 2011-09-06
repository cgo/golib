/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <godenoise.h>
#ifndef GOMATHSIGNAL_H
# include <gomathsignal.h>
#endif
#include <gosignal3dgenericiterator.h>

#define NAMESPACE goSignal

namespace NAMESPACE
{
    class TVL1Private
    {
        public:
            TVL1Private () 
                : f (0), u (0), lambda (1.0),
                  autoTimeStep (true),
                  Dx (), Dxx (), Dy ()
            {};
            ~TVL1Private () {};

            //= Call when f has been set -- initialise u and the temp storage Dx, Dy, Dxx.
            bool initialise ();

            // goDouble energy (goSignalBase<void>& update, goDouble dt);

            goAutoPtr<goSignal3DBase<void> > f;
            goAutoPtr<goSignal3DBase<void> > u;
            goDouble lambda;

            bool autoTimeStep;

            goSignal3D<void> Dx, Dxx, Dy;
    };
};

bool NAMESPACE ::TVL1Private::initialise ()
{
    if (this->f.isNull ())
    {
        Dx.destroy ();
        Dy.destroy ();
        Dxx.destroy ();
        u.set (0);
        return true;
    }

    //= Copy f to u
    if (u.isNull ())
    {
        u.set (new goSignal3D<void>);
    }

    //= Initialise u with f
    ((goSignal3D<void>*)(u.get ())) -> copy (*f);

    if (u->getSize () != f->getSize ())
    {
        goLog::warning ("NAMESPACE ::TVL1: u and f sizes must match.");
        return false;
    }

    //= Create temporary storage for regularisation term
    if (this->Dx.getSize () != u->getSize () ||
        this->Dx.getDataType().getID () != u->getDataType().getID ())
    {
        this->Dx.setDataType (u->getDataType().getID ());
        this->Dxx.setDataType (u->getDataType().getID ());
        this->Dy.setDataType (u->getDataType().getID ());
        this->Dx.make (u);
        this->Dxx.make (u);
        this->Dy.make (u);
    }

    return true;
}

NAMESPACE ::TVL1::TVL1 (goAutoPtr<goSignal3DBase<void> > f, goDouble lambda)
    : myPrivate (0)
{
    myPrivate = new TVL1Private;
    myPrivate->f = f;
    myPrivate->lambda = lambda;
    myPrivate->initialise ();
}

NAMESPACE ::TVL1::~TVL1 ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/** 
 * @brief Set if the time step should be determined automatically.
 * 
 * By default, this is true.
 *
 * @param t Flag for automatic time step determination.
 */
void NAMESPACE ::TVL1::setAutoTimeStep (bool t)
{
    myPrivate->autoTimeStep = t;
}

/** 
 * @brief Set weight lambda for the data term.
 * 
 * @param l Weight.
 */
void NAMESPACE ::TVL1::setLambda (goDouble l)
{
    myPrivate->lambda = l;
}

/** 
 * @brief Get weight lambda for the data term.
 * 
 * @return Weight of the data term.
 */
goDouble NAMESPACE ::TVL1::getLambda () const
{
    return myPrivate->lambda;
}

/*
 * @brief Do one step of evolution.
 * 
 * Updates u_n to u_{n+1}.
 *
 * @param myPrivate TVL1Private object to use.
 * @param dt        Discrete time step.
 * 
 * @return True if successful, false otherwise.
 */
template <class Tf, class Tu>
static bool TVL1_evolve_2 (NAMESPACE ::TVL1Private* myPrivate, goDouble dt)
{
    //= Non-threaded version.
    goMath::forwardDifferences (*myPrivate->u, myPrivate->Dx, 0);
    goMath::forwardDifferences (*myPrivate->u, myPrivate->Dy, 1);

    //= Normalise gradient
    {
        goFloat temp = 0.0;
        goFloat epsilon = 0.001;
        goSignal3DGenericIterator x (&myPrivate->Dx), y (&myPrivate->Dy);
        while (!x.endY())
        {
            x.resetX(); y.resetX();
            while (!x.endX())
            {
                temp = ::sqrt(*(goFloat*)*x * *(goFloat*)*x + *(goFloat*)*y * *(goFloat*)*y + epsilon);
                *(goFloat*)*x /= temp;
                *(goFloat*)*y /= temp;
                x.incrementX(); y.incrementX();
            }
            x.incrementY(); y.incrementY();
        }
    }

    //= Non-threaded version:
    //= This can be optimised, but this version is clearer.
    goMath::backwardDifferences (myPrivate->Dx, myPrivate->Dxx, 0);
    goMath::backwardDifferences (myPrivate->Dy, myPrivate->Dx, 1);
    myPrivate->Dxx += myPrivate->Dx;

    {
        goFloat epsilon = 0.001;
        goSignal3DGenericIterator itf (myPrivate->f);
        goSignal3DGenericIterator itu (myPrivate->u);
        goSignal3DGenericIterator target (&myPrivate->Dxx);
        Tu f (Tf(0));
        Tu u (Tu(0));
        while (!itf.endY())
        {
            itf.resetX ();
            itu.resetX ();
            target.resetX ();
            while (!itu.endX())
            {
                f = *(Tf*)*itf;
                u = *(Tu*)*itu;
                *(Tu*)*target += myPrivate->lambda * (f - u) / (::fabs (f - u) + epsilon);
                itf.incrementX ();
                itu.incrementX ();
                target.incrementX ();
            }
            itf.incrementY ();
            itu.incrementY ();
            target.incrementY ();
        }
    }

    if (myPrivate->autoTimeStep)
    {
        goDouble min = 0.0, max = 0.0;
        myPrivate->Dxx.getMinMax (min, max);
        goDouble m = goMath::max (::fabs (min), ::fabs (max));
        if (m != 0.0)
            m = 0.01 / m;
        else
            m = dt;

        myPrivate->Dxx *= m;
    }
    else
    {
        myPrivate->Dxx *= dt;
    }
    *myPrivate->u += myPrivate->Dxx;

    return true;
}

/*
 * @brief Proxy for TVL1_evolve_2. See there.
 * 
 * @param myPrivate 
 * @param dt 
 * 
 * @return 
 */
template <class Tf>
static bool TVL1_evolve_1 (NAMESPACE ::TVL1Private* myPrivate, goDouble dt)
{
    switch (myPrivate->u->getDataType().getID ())
    {
        case GO_FLOAT: return TVL1_evolve_2<Tf, goFloat> (myPrivate, dt); break;
        case GO_DOUBLE: return TVL1_evolve_2<Tf, goDouble> (myPrivate, dt); break;
        default: goLog::warning ("NAMESPACE ::TVL1::evolve (): Only goFloat and goDouble allowed for u.");
                 break;
    }

    return false;
}

/**
 * @brief Do one step of evolution.
 * 
 * Updates u_n to u_{n+1}.
 *
 * @param dt        Discrete time step.
 * 
 * @return True if successful, false otherwise.
 */
bool NAMESPACE ::TVL1::evolve (goDouble dt)
{
    switch (myPrivate->f->getDataType().getID ())
    {
        case GO_FLOAT:  return TVL1_evolve_1 <goFloat> (myPrivate, dt); break;
        case GO_DOUBLE: return TVL1_evolve_1 <goDouble> (myPrivate, dt); break;
        default: goLog::warning ("NAMESPACE ::TVL1::evolve (): only goFloat and goDouble data types allowed.");
                 break;
    }

    return false;
}

/** 
 * @brief Get the current regularised image u.
 * 
 * @return goAutoPtr to the current estimate of the image, u.
 */
goAutoPtr<goSignal3DBase<void> > NAMESPACE ::TVL1::getU ()
{
    return myPrivate->u;
}
