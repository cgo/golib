#include <gogaussimage.h>
#include <gosignal3d.h>
#include <gosignal3dgenericiterator.h>
#include <golog.h>

class goGaussImagePrivate
{
    public:
        goGaussImagePrivate ()
            : mu(), sigma2(), mean_squared(), counter(0)
        {
            mu.setDataType (GO_FLOAT);
            sigma2.setDataType (GO_FLOAT);
            mean_squared.setDataType (GO_FLOAT);

            mu.make (goSize3D(1,1,1), goSize3D(1,1,1), goSize3D(0,0,0), 1);
            sigma2.make (goSize3D(1,1,1), goSize3D(1,1,1), goSize3D(0,0,0), 1);
            mean_squared.make (goSize3D(1,1,1), goSize3D(1,1,1), goSize3D(0,0,0), 1);

        };
        ~goGaussImagePrivate () {};

        goSignal3D<void> mu;
        goSignal3D<void> sigma2;
        goSignal3D<void> mean_squared;
        goSize_t         counter;
};

goGaussImage::goGaussImage ()
    : myPrivate (0)
{
    myPrivate = new goGaussImagePrivate;
}

goGaussImage::~goGaussImage ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

template <class T>
static void fillSignal (goSignal3DBase<void>& s, goFloat v)
{
    T vv = static_cast<T> (v);
    s.fill (&vv);
}

template <class T>
static void update_mean_squared (goFloat f1, goSignal3D<void>& mean_squared, const goSignal3DBase<void>& u, goFloat f2)
{
    goSignal3DGenericIterator it (&mean_squared);
    goSignal3DGenericConstIterator itU (&u);
    while (!it.endZ())
    {
        it.resetY();
        itU.resetY();
        while (!it.endY())
        {
            it.resetX();
            itU.resetX();
            while (!it.endX())
            {
                goFloat u_val = static_cast<goFloat>(*(T*)*itU);
                *(goFloat*)*it = (*(goFloat*)*it * f1 + u_val * u_val) * f2;
                it.incrementX();
                itU.incrementX();
            }
            it.incrementY();
            itU.incrementY();
        }
        it.incrementZ();
        itU.incrementZ();
    }
}

template <class T>
static void update_mu_sigma_squared (goFloat k, goSignal3DBase<void>& sigma2, goSignal3DBase<void>& mean_squared, goSignal3DBase<void>& mu, const goSignal3DBase<void>& u)
{
    goSignal3DGenericIterator      it_mu  (&mu);
    goSignal3DGenericIterator      it_msq (&mean_squared);
    goSignal3DGenericIterator      it_sigma2 (&sigma2);
    goSignal3DGenericConstIterator it_u (&u);

    goFloat f1 = k;
    goFloat f2 = 1.0f / (k + 1.0f);

    while (!it_msq.endZ())
    {
        it_msq.resetY();
        it_u.resetY();
        it_sigma2.resetY();
        it_mu.resetY();
        while (!it_msq.endY())
        {
            it_msq.resetX();
            it_u.resetX();
            it_sigma2.resetX();
            it_mu.resetX();
            while (!it_msq.endX())
            {
                goFloat u_val = static_cast<goFloat>(*(T*)*it_u);
                goFloat new_mu = (*(goFloat*)*it_mu * f1 + u_val) * f2;
                *(goFloat*)*it_mu = new_mu;

                goFloat new_msq = (*(goFloat*)*it_msq * f1 + u_val * u_val) * f2;
                *(goFloat*)*it_msq = new_msq;

                *(goFloat*)*it_sigma2 = new_msq - new_mu * new_mu;
                it_msq.incrementX();
                it_u.incrementX();
                it_sigma2.incrementX();
                it_mu.incrementX();
            }
            it_msq.incrementY();
            it_u.incrementY();
            it_sigma2.incrementY();
            it_mu.incrementY();
        }
        it_msq.incrementZ();
        it_u.incrementZ();
        it_sigma2.incrementZ();
        it_mu.incrementZ();
    }
}

void goGaussImage::update (const goSignal3DBase<void>& u)
{
    goSignal3D<void>& mu = myPrivate->mu;
    goSignal3D<void>& sigma2 = myPrivate->sigma2;
    goSignal3D<void>& mean_squared = myPrivate->mean_squared;
    if (mu.getSize() != u.getSize())
    {
        mu.make (&u);
        sigma2.make (&u);
        mean_squared.make (&u);
        this->reset ();
    }

    goFloat k = static_cast<goFloat>(myPrivate->counter);
    switch (u.getDataType().getID())
    {
        case GO_UINT8:  update_mu_sigma_squared<goUInt8>  (k, sigma2, mean_squared, mu, u); break;
        case GO_INT8:   update_mu_sigma_squared<goInt8>   (k, sigma2, mean_squared, mu, u); break;
        case GO_UINT16: update_mu_sigma_squared<goUInt16> (k, sigma2, mean_squared, mu, u); break;
        case GO_INT16:  update_mu_sigma_squared<goInt16>  (k, sigma2, mean_squared, mu, u); break;
        case GO_UINT32: update_mu_sigma_squared<goUInt32> (k, sigma2, mean_squared, mu, u); break;
        case GO_INT32:  update_mu_sigma_squared<goInt32>  (k, sigma2, mean_squared, mu, u); break;
        case GO_FLOAT:  update_mu_sigma_squared<goFloat>  (k, sigma2, mean_squared, mu, u); break;
        case GO_DOUBLE: update_mu_sigma_squared<goDouble> (k, sigma2, mean_squared, mu, u); break;
        default: goLog::warning ("goGaussImage::update(): unsupported type."); 
                 return; break;
    }
    ++myPrivate->counter;
}

void goGaussImage::reset ()
{
    goFloat zero = 0.0f;
    myPrivate->sigma2.fill (&zero);
    myPrivate->mean_squared.fill (&zero);
    myPrivate->counter = 0;
}

const goSignal3DBase<void>& goGaussImage::getMean () const
{
    return myPrivate->mu;
}

const goSignal3DBase<void>& goGaussImage::getVariance () const
{
    return myPrivate->sigma2;
}
