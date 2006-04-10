#include <goregionls.h>
#include <assert.h>
#include <golog.h>
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif
#ifndef GOSIGNAL3DGENERICITERATOR_H
# include <gosignal3dgenericiterator.h>
#endif
#ifndef GOSIGNALHELPER_H
# include <gosignalhelper.h>
#endif
#ifndef GOSIGNALMACROS_H
# include <gosignalmacros.h>
#endif
#ifndef GOMATH_H
# include <gomath.h>
#endif

#ifndef GOSPARSEMATRIX_H
# include <gosparsematrix.h>
#endif

//#define HAVE_MATLAB
//#include <gomatlab.h>

class goRegionLSPrivate
{
    public:
        goRegionLSPrivate ();
        ~goRegionLSPrivate ();

        goDouble mu;
        goDouble nu;
        goDouble lambda1;
        goDouble lambda2;

        goDouble epsilon;
        
        goSignal3DBase<void>* image;
        goSignal3D<void>      phi;
        goDouble              hx;
        goDouble              hy;
};

goRegionLSPrivate::goRegionLSPrivate ()
    : mu (0.1),
      nu (0.0),
      lambda1 (1.0),
      lambda2 (1.0),
      epsilon (1.0),
      image (0),
      phi (),
      hx  (1.0),
      hy  (1.0)
{
    phi.setDataType (GO_DOUBLE);
}

goRegionLSPrivate::~goRegionLSPrivate ()
{
}

goRegionLS::goRegionLS ()
 : goObjectBase()
{
    this->setClassName ("goRegionLS");
    this->myPrivate = new goRegionLSPrivate;
    assert (this->myPrivate);
}

goRegionLS::~goRegionLS ()
{
    if (myPrivate)
    {
        delete myPrivate;
        myPrivate = 0;
    }
}

/** --------------------------------------------------------------------------
 * @brief Sets the grey value image.
 * 
 * @param signal  Contains the (2D) image data. Must be gray values
 *                and either of type float or double (currently, this MUST be double!)
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goRegionLS::setImage (goSignal3DBase<void>* signal)
{
    if (!signal)
    {
        myPrivate->phi.destroy();
    }
    else
    {
        if (signal->getDataType().getID() != GO_FLOAT &&
            signal->getDataType().getID() != GO_DOUBLE)
        {
            goLog::error ("Image data type must be float or double",this);
            return false;
        }
        myPrivate->image = signal;
        myPrivate->phi.make (signal->getSizeX(),
                             signal->getSizeY(),
                             signal->getSizeZ(),
                             signal->getBlockSizeX(),
                             signal->getBlockSizeY(),
                             signal->getBlockSizeZ(),
                             4, 4, 4);
        //goDouble hx = 1.0 / (float)signal->getSizeX();
        //goDouble hy = 1.0 / (float)signal->getSizeY();
        goDouble hx = 1.0;
        goDouble hy = 1.0;
        myPrivate->hx = hx;
        myPrivate->hy = hy;
        goSignal3DGenericIterator it (&myPrivate->phi);
        goFloat radius = 0.25f * goMath::max(signal->getSizeX() * hx, signal->getSizeY() * hy);
        goPointf center;
        center.x = signal->getSizeX() * 0.5f * hx;
        center.y = signal->getSizeY() * 0.5f * hy;
        while (!it.endZ())
        {
            goFloat y = -center.y;
            it.resetY();
            while (!it.endY())
            {
                goFloat x = -center.x;
                it.resetX();
                while (!it.endX())
                {
                    *(goDouble*)*it = sqrt(x*x+y*y) - radius;
                    x += hx;
                    it.incrementX();
                }
                y += hy;
                it.incrementY();
            }
            it.incrementZ();
        }
//        GO_SIGNAL3D_EACHELEMENT_GENERIC (*(goDouble*)__ptr = 0.0, (myPrivate->phi));
    }
    return true;
}

/** --------------------------------------------------------------------------
 * @brief Sets the mu parameter.
 * 
 * @param mu  The value multiplied to the curvature term. Defaults to 1.
 ----------------------------------------------------------------------------*/
void goRegionLS::setMu (goDouble mu)
{
    myPrivate->mu = mu;
}

/** --------------------------------------------------------------------------
 * @brief Sets the nu parameter.
 * 
 * @param nu  Area term. Defaults to 0.
 ----------------------------------------------------------------------------*/
void goRegionLS::setNu (goDouble nu)
{
    myPrivate->nu = nu;
}

/** --------------------------------------------------------------------------
 * @brief Set multiplier for interior term.
 * 
 * @param l  Defaults to 1.
 ----------------------------------------------------------------------------*/
void goRegionLS::setLambda1 (goDouble l)
{
    myPrivate->lambda1 = l;
}

/** --------------------------------------------------------------------------
 * @brief Set multiplier for exterior term.
 * 
 * @param l  Defaults to 1.
 ----------------------------------------------------------------------------*/
void goRegionLS::setLambda2 (goDouble l)
{
    myPrivate->lambda2 = l;
}

/** 
 * @brief Set epsilon value for the regularised heaviside and dirac functions.
 * 
 * @param e Epsilon. Defaults to 1.0.
 */
void goRegionLS::setEpsilon (goDouble e)
{
    myPrivate->epsilon = e;
}

void goRegionLS::setHx (goDouble h)
{
    myPrivate->hx = h;
}

void goRegionLS::setHy (goDouble h)
{
    myPrivate->hy = h;
}

/** --------------------------------------------------------------------------
 * @brief Get the current Phi grid.
 * 
 * @return Reference to the signal (2D) which contains the current Phi
 *         grid values. Should always be of type goDouble.
 ----------------------------------------------------------------------------*/
goSignal3DBase<void>* goRegionLS::getPhi ()
{
    return &myPrivate->phi;
}

/* --------------------------------------------------------------------------
 * @brief Regularised Heaviside function.
 * 
 * @param x 
 * @param epsilon 
 * 
 * @return 
 ----------------------------------------------------------------------------*/
static goDouble heaviside (goDouble x, goDouble epsilon)
{
    static goDouble co = 2.0 / (goDouble)M_PI;
//    if (x < -epsilon)
//        return 0.0;
//    if (x > epsilon)
//        return 1.0;
    return 0.5*(1.0+co*atan(x/epsilon));
}

/* --------------------------------------------------------------------------
 * @brief Regularised dirac pulse.
 * 
 * @param x 
 * @param epsilon 
 * 
 * @return 
 ----------------------------------------------------------------------------*/
static goDouble dirac (goDouble x, goDouble epsilon)
{
    return 0.3183098861837907/(epsilon*(1 + x*x/(epsilon*epsilon)));
}

/** --------------------------------------------------------------------------
 * @brief Calculate the Chan&Vese term for current Phi and image.
 * 
 * Calculates dirac(Phi) * (mu*div*(nabla(Phi)/|nabla(Phi)| - nu - lambda1*(image - c1) + lambda2*(image - c2))), 
 * with c1 and c2 the mean inner and outer grey values of the image.
 * 
 * @param result  Must be of the image's size and of type goDouble. Contains the result for
 *                the above equation. Note that this is not yet multiplied by a time step.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goRegionLS::chanVeseTerm (goSignal3DBase<void>& result)
{
    goDouble c2 = this->innerMean();
    goDouble c1 = this->outerMean();

    goSignal3D<void> nablaPhiX;
    nablaPhiX.setDataType (GO_DOUBLE);
    goSignal3D<void> nablaPhiY;
    nablaPhiY.setDataType (GO_DOUBLE);
    if (!goMath::centralDifferences(myPrivate->phi, nablaPhiX, 0, myPrivate->hx))
        return false;
    if (!goMath::centralDifferences(myPrivate->phi, nablaPhiY, 1, myPrivate->hy))
        return false;
    goSignal3DGenericIterator itX(&nablaPhiX);
    goSignal3DGenericIterator itY(&nablaPhiY);

//	goDouble mean = 0.0, variance = 0.0;
//	goSignalMeanVariance (nablaPhiX, mean, variance);
//	printf ("Mean/variance of nablaPhiX: %f/%f\n", mean, variance);
//    goString desc;
//    goSignalInfoText (nablaPhiX, desc);
//    printf ("nablaPhiX: %s\n", desc.toCharPtr());
//	goSignalMeanVariance (myPrivate->phi, mean, variance);
//	printf ("Mean/variance of phi: %f/%f\n", mean, variance);
//    goSignalInfoText (myPrivate->phi, desc);
//    printf ("phi: %s\n", desc.toCharPtr());


    //= nablaPhi/|nablaPhi|
    while (!itX.endZ())
    {
        itX.resetY();
        itY.resetY();
        while (!itX.endY())
        {
            itX.resetX();
            itY.resetX();
            while (!itX.endX())
            {
                goDouble temp = sqrt(*(goDouble*)*itX * *(goDouble*)*itX + 
                                *(goDouble*)*itY * *(goDouble*)*itY);
                if (temp != 0.0)
                    temp = 1.0 / temp;
                *(goDouble*)*itX = *(goDouble*)*itX * temp;
                *(goDouble*)*itY = *(goDouble*)*itY * temp;
                itX.incrementX();
                itY.incrementX();
            }
            itX.incrementY();
            itY.incrementY();
        }
        itX.incrementZ();
        itY.incrementZ();
    }

    goSignal3D<void> div;

    if (!goMath::divergence (nablaPhiX, nablaPhiY, myPrivate->hx, myPrivate->hy, div))
        return false;

//    myPrivate->matlab.signalToVariable (&nablaPhiX, "x");
//    myPrivate->matlab.signalToVariable (&nablaPhiY, "y");
//    myPrivate->matlab.signalToVariable (&div, "div");
//    goString matlabResult;
//    matlabResult.resize (1024);
/*    myPrivate->matlab.matlabCall ("div2 = divergence(x,y);\
                                   mean(mean(abs(div-div2))),\
                                   figure(1),imagesc(div2),colorbar;\
                                   figure(2),imagesc(div),colorbar;\
                                   waitforbuttonpress;", &matlabResult);
*/  
//    printf ("%s\n",matlabResult.toCharPtr());
//    goSignal3D<void> div2;
//    if (!myPrivate->matlab.variableToSignal (&div2, "div2"))
//    {
//        goLog::error ("Could not get div from matlab.",this);
//        return false;
//    }
    
    
    //= Now do div - nu - lambda1*(image - c1)^2 + lambda2*(image - c2)^2.
    //= times dirac(phi).
    if (!goCopySignal(&div,&result))
    {
        goLog::error ("chanVeseTerm(): could not copy div to result.",this);
        return false;
    }
    result *= myPrivate->mu;
    result -= myPrivate->nu;
    if (myPrivate->image->getDataType().getID() != GO_DOUBLE)
    {
        goLog::error ("goRegionLS: currently, image MUST be goDouble.", this);
        return false;
    }

    goSignal3DGenericIterator itImage (myPrivate->image);
    goSignal3DGenericIterator itResult (&result);
    goSignal3DGenericIterator itPhi (&myPrivate->phi);
    
    while (!itImage.endZ())
    {
        itImage.resetY();
        itResult.resetY();
        itPhi.resetY();
        while (!itImage.endY())
        {
            itImage.resetX();
            itResult.resetX();
            itPhi.resetX();
            while (!itImage.endX())
            {
                goDouble l1 = *(goDouble*)*itImage - c1;
                l1 *= l1;
                goDouble l2 = *(goDouble*)*itImage - c2;
                l2 *= l2;
                *(goDouble*)*itResult += -myPrivate->lambda1 * l1 + myPrivate->lambda2 * l2;
                *(goDouble*)*itResult *= dirac (*(goDouble*)*itPhi, myPrivate->epsilon);
                itImage.incrementX();
                itResult.incrementX();
                itPhi.incrementX();
            }
            itImage.incrementY();
            itResult.incrementY();
            itPhi.incrementY();
        }
        itImage.incrementZ();
        itResult.incrementZ();
        itPhi.incrementZ();
    }
    return true;
}

/** --------------------------------------------------------------------------
 * @brief Evolves one time step.
 * 
 * @param deltaT Time step. Note that there is a restriction on the step since
 *               the PDE is solved explicitly.
 * 
 * @return True if successful, false otherwise.
 ----------------------------------------------------------------------------*/
bool goRegionLS::evolve (goDouble deltaT)
{
    //= delta_phi = chan-vese-term;
    //= phi = phi + deltaT * delta_phi;
    //= dirac_epsilon(phi) * (div(nablaPhi/|nablaPhi|) - nu - 
    //=                                 lambda1 * (image - c1)^2 + 
    //=                                 lambda2 * (image - c2)^2)
    
    goSignal3D<void> chanVese;
    chanVese.setDataType(GO_DOUBLE);
    goSignal3DBase<void>* i = myPrivate->image;
    chanVese.make (i->getSizeX(), i->getSizeY(), i->getSizeZ(),
                   i->getBlockSizeX(), i->getBlockSizeY(), i->getBlockSizeZ(),
                   4,4,4);
    if (!this->chanVeseTerm(chanVese))
    {
        goLog::warning("chanVeseTerm() failed.",this);
        return false;
    }
//	goDouble mean = 0.0;
//	goDouble variance = 0.0;
//	goSignalMeanVariance (chanVese, mean, variance);
//	printf ("chanVese mean/variance before mult: %f/%f\n", mean, variance);
    chanVese *= deltaT;
    myPrivate->phi += chanVese;
    
    return true;
}

#define HAVE_MATLAB
#include "mex.h"
#include "engine.h"
#include <gomatlab.h>

bool goRegionLS::evolveImplicitly (goDouble deltaT)
{
    static goMatlab matlab;
    goSignal3DBase<void>* i = myPrivate->image;
    assert(i);
    
    //= Assemble linear equation system for solving the pde implicitly.
    assert (myPrivate->phi.getSizeX() == i->getSizeX());
    assert (myPrivate->phi.getSizeY() == i->getSizeY());

    goUInt32 sizeX = i->getSizeX();
    goUInt32 sizeY = i->getSizeY();
    goUInt32 N = sizeX * sizeY;
    goSparseMatrix A(N,N);
    goVectord      b(N);
    goSignal3DGenericIterator itPhi(&myPrivate->phi);
    goSignal3DGenericIterator itImage(myPrivate->image);
    A.fillBegin(5*N);
    goIndex_t idx = 0;

    goDouble c2 = this->innerMean();
    goDouble c1 = this->outerMean();
 
    printf ("c1 (outer) == %f\nc2 (inner) == %f\n",c1,c2);
    
    goIndex_t y = 0;
    while(!itPhi.endY())
    {
        itPhi.resetX();
        itImage.resetX();
        goIndex_t x = 0;

        //= ============== Was tun am Rand? ===============
        
        while (!itPhi.endX())
        {
            goDouble diracPhi = dirac(*(goDouble*)*itPhi,myPrivate->epsilon);
            // temp1 = Delta_+^x(Phi^n_{i,j})^2 / h_x^2
            goDouble temp1 = *(goDouble*)itPhi.rightX() - *(goDouble*)*itPhi;
            temp1 = temp1*temp1 / (myPrivate->hx*myPrivate->hx);
            // temp2 = (Phi^n_{i,j+1} - Phi^n_{i,j-1})^2 / (2h_y)^2
            goDouble temp2 = *(goDouble*)itPhi.rightY() - *(goDouble*)itPhi.leftY();
            temp2 = temp2*temp2 / (4*myPrivate->hy*myPrivate->hy);
            goDouble mu_x = 0.0;
            if (temp1 + temp2 != 0.0)
            {
                mu_x = myPrivate->mu / (myPrivate->hx*myPrivate->hx)
                    * deltaT
                    / sqrt(temp1 + temp2) * diracPhi;
            }

            temp1 = *(goDouble*)itPhi.rightX() - *(goDouble*)itPhi.leftX();
            temp1 = temp1*temp1 / (4*myPrivate->hx*myPrivate->hx);
            temp2 = *(goDouble*)itPhi.rightY() - *(goDouble*)*itPhi;
            temp2 = temp2*temp2 / (myPrivate->hy*myPrivate->hy);
            goDouble mu_y = 0.0;
            if (temp1 + temp2 != 0.0)
            {
                mu_y = myPrivate->mu / (myPrivate->hy*myPrivate->hy)
                    * deltaT
                    / sqrt(temp1 + temp2) * diracPhi;
            }

            // RHS: b = phi + diracPhi * area+greyvalueterms * deltaT
            goDouble l1 = *(goDouble*)*itImage - c1;
            l1 *= l1;
            goDouble l2 = *(goDouble*)*itImage - c2;
            l2 *= l2;
            b[idx] = (*(goDouble*)*itPhi + diracPhi * deltaT * (-myPrivate->nu - myPrivate->lambda1 * l1 + myPrivate->lambda2 * l2));
            // LHS: calculate lhs of the equation
            // i+1,j
            if (x < (goIndex_t)(sizeX-1))
                A.fillNext(idx,y*sizeX+x+1,  -mu_x);
            else
                A.fillNext(idx,y*sizeX,-mu_x);
            // i,j
            A.fillNext(idx,y*sizeX+x,    1+2*mu_x+2*mu_y);
            // i-1,j
            if (x >= 1)
                A.fillNext(idx,y*sizeX+x-1,  -mu_x);
            else
                A.fillNext(idx,y*sizeX+sizeX-1,-mu_x);
            // i,j+1
            if (y < (goIndex_t)(sizeY-1))
                A.fillNext(idx,(y+1)*sizeX+x,-mu_y);
            else
                A.fillNext(idx,x,-mu_y);
            // i,j-1
            if (y >= 1)
                A.fillNext(idx,(y-1)*sizeX+x,-mu_y);
            else
                A.fillNext(idx,(sizeY-1)*sizeX+x,-mu_y);
            
            itPhi.incrementX();
            itImage.incrementX();
            ++idx;
            ++x;
        }
        itPhi.incrementY();
        itImage.incrementY();
        ++y;
    }
    A.fillEnd();

    // Solve Ax=b for x, where x is Phi_{n+1}
    goVectord newPhi(N);
    newPhi.fill(0.0);
    //matlab.putSparse (&A, "A");
    //matlab.putVector (&b, "b");
    //goString buffer;
    //buffer.resize(1024);
    //matlab.matlabCall ("x = pcg(A,b);",&buffer);
    // printf ("%s\n",buffer.toCharPtr());
    //matlab.getVector (&newPhi, "x");
    if (goMath::goConjugateGradients(A,b,newPhi,1e-6) > 1e-6)
    {
        goLog::warning("goConjugateGradients() failed.",this);
        return false;
    }

    goCopySignalArray (newPhi.getPtr(), &myPrivate->phi);
    // Below is the explicit copying. Same thing.
#if 0 
    goDouble* phiP = newPhi.getPtr();
    itPhi.resetX();
    itPhi.resetY();
    itPhi.resetZ();
    while (!itPhi.endY())
    {
        itPhi.resetX();
        while (!itPhi.endX())
        {
            *(goDouble*)*itPhi = *phiP;
            itPhi.incrementX();
            ++phiP;
        }
        itPhi.incrementY();
    }
#endif
//    matlab.signalToVariable (&myPrivate->phi, "phi");
//    matlab.matlabCall ("subplot(2,1,1); contour(phi,[0 0]); waitforbuttonpress;");
//    matlab.matlabCall ("subplot(2,1,2); surf(phi); waitforbuttonpress;");
    return true;
}

template <class T>
static goDouble outerMean_ (const goSignal3DBase<void>& image, const goSignal3DBase<void>& phi, goDouble epsilon)
{
    assert (phi.getDataType().getID() == GO_DOUBLE);
    
    goSignal3DGenericConstIterator itImage (&image);
    goSignal3DGenericConstIterator itPhi   (&phi);

    goDouble mean = 0.0;
    goDouble area = 0.0;
    goDouble h = 0.0;
    while (!itImage.endZ())
    {
        itImage.resetY();
        itPhi.resetY();
        while (!itImage.endY())
        {
            itImage.resetX();
            itPhi.resetX();
            while (!itImage.endX())
            {
                h = heaviside (*(const goDouble*)*itPhi, epsilon);
                area += h;
                mean += *(const T*)*itImage * h;
                itImage.incrementX();
                itPhi.incrementX();
            }
            itImage.incrementY();
            itPhi.incrementY();
        }
        itImage.incrementZ();
        itPhi.incrementZ();
    }
    if (area != 0.0)
    {
        return mean / area;
    }
    else
    {
        return 0.0;
    }
}

template <class T>
static goDouble innerMean_ (const goSignal3DBase<void>& image, const goSignal3DBase<void>& phi, goDouble epsilon)
{
    assert (phi.getDataType().getID() == GO_DOUBLE);
    
    goSignal3DGenericConstIterator itImage (&image);
    goSignal3DGenericConstIterator itPhi   (&phi);

    goDouble mean = 0.0;
    goDouble area = 0.0;
    goDouble h = 0.0;
    while (!itImage.endZ())
    {
        itImage.resetY();
        itPhi.resetY();
        while (!itImage.endY())
        {
            itImage.resetX();
            itPhi.resetX();
            while (!itImage.endX())
            {
                h = 1.0 - heaviside (*(const goDouble*)*itPhi, epsilon);
                area += h;
                mean += *(const T*)*itImage * h;
                itImage.incrementX();
                itPhi.incrementX();
            }
            itImage.incrementY();
            itPhi.incrementY();
        }
        itImage.incrementZ();
        itPhi.incrementZ();
    }
    if (area != 0.0)
    {
        return mean / area;
    }
    else
    {
        return 0.0;
    }
}

/** --------------------------------------------------------------------------
 * @brief Calculate the inner mean.
 * 
 * @return Inner mean (where Phi > 0)
 ----------------------------------------------------------------------------*/
goDouble goRegionLS::innerMean ()
{
    if (!myPrivate->image)
    {
        return 0.0;
    }
    switch (myPrivate->image->getDataType().getID())
    {
        case GO_FLOAT: return innerMean_<goFloat> (*myPrivate->image, myPrivate->phi, myPrivate->epsilon); break;
        case GO_DOUBLE: return innerMean_<goDouble> (*myPrivate->image, myPrivate->phi, myPrivate->epsilon); break;
        default:
        {
            goLog::error ("innerMean(): unsupported image data type.",this);
        }
        break;
    }
    return 0.0;
}

/** --------------------------------------------------------------------------
 * @brief Calculate the outer mean.
 * 
 * @return Outer mean (where Phi < 0)
 ----------------------------------------------------------------------------*/
goDouble goRegionLS::outerMean ()
{
    if (!myPrivate->image)
    {
        return 0.0;
    }
    switch (myPrivate->image->getDataType().getID())
    {
        case GO_FLOAT: return outerMean_<goFloat> (*myPrivate->image, myPrivate->phi, myPrivate->epsilon); break;
        case GO_DOUBLE: return outerMean_<goDouble> (*myPrivate->image, myPrivate->phi, myPrivate->epsilon); break;
        default:
        {
            goLog::error ("innerMean(): unsupported image data type.",this);
        }
        break;
    }
    return 0.0;
}
