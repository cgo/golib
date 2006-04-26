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

#include <gotimerobject.h>
#include <gofilter3d.h>

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

        goDouble delingetteFactor;
        goDouble liFactor;
        
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
      delingetteFactor (false),
      liFactor (0.0),
      image (0),
      phi (),
      hx  (1.0),
      hy  (1.0)
{
    phi.setDataType (GO_DOUBLE);
    phi.setBorderFlags (GO_X|GO_Y|GO_Z, GO_CONSTANT_BORDER);
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

template <class T> 
static void initStep2 (goSignal3DBase<void>& target)
{
    goSignal3DGenericIterator it(&target);
    const T value1 = T(0.0);
    const T value2 = T(1.0);
    T value = value1;
    while (!it.endY())
    {
        it.resetY();
        while (!it.endX())
        {
            if (*(T*)*it == value2)
            {
                if (value == value1)
                    value = value2;
                else
                    value = value1;
            }
            else
            {
                *(T*)*it = value;
            }
            it.incrementX();
        }
        it.incrementY();
    }
}

/** 
* @todo Write step initialisation for testing
*       the Li-term and free initialisation from
*       things like edge detection.
* @param target 
*/
static void initStep (goSignal3DBase<void>& target)
{
    switch (target.getDataType().getID())
    {
        case GO_FLOAT: initStep2<goFloat>(target); break;
        case GO_DOUBLE: initStep2<goDouble>(target); break;
        default: goLog::warning ("goRegionLS, initStep(): target must be float or double."); break;
    }
}

template <class T>
static void initCircle (goFloat r, goDouble hx, goDouble hy, goSignal3DBase<void>& target)
{
    goSignal3DGenericIterator it (&target);
    goFloat radius = r * goMath::max(target.getSizeX() * hx, target.getSizeY() * hy);
    goPointf center;
    center.x = target.getSizeX() * 0.5f * hx;
    center.y = target.getSizeY() * 0.5f * hy;
    printf ("initCircle(): center = (%f %f)\n",center.x,center.y);
    printf (" (hx,hy) = (%f %f)\n",hx,hy);
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
                *(T*)*it = sqrt(x*x+y*y) - radius;
                x += hx;
                it.incrementX();
            }
            y += hy;
            it.incrementY();
        }
        it.incrementZ();
    }
}

template <class T>
static void initQuad (goFloat r, goDouble hx, goDouble hy, goSignal3DBase<void>& target)
{
    goSignal3DGenericIterator it (&target);
    goIndex_t rx = static_cast<goIndex_t>(r * target.getSizeX());
    goIndex_t ry = static_cast<goIndex_t>(r * target.getSizeY());
    goPointf center;
    center.x = target.getSizeX() * 0.5f;
    center.y = target.getSizeY() * 0.5f;
    goFloat maxX = static_cast<goFloat>(target.getSizeX());
    goFloat maxY = static_cast<goFloat>(target.getSizeY());
    while (!it.endZ())
    {
        goFloat y = 0.0f;
        it.resetY();
        while (!it.endY())
        {
            goFloat x = 0.0f;
            it.resetX();
            while (!it.endX())
            {
                goDouble d1 = x;
                goDouble d2 = maxX - x;
                goDouble d3 = y;
                goDouble d4 = maxY - y;
                *(T*)*it = goMath::min (goMath::min(d1,d2),goMath::min(d3,d4)) 
                    - goMath::min(rx,ry);
                x += 1.0f;
                it.incrementX();
            }
            y += 1.0f;
            it.incrementY();
        }
        it.incrementZ();
    }
}

/** 
 * @brief Sets the grey value image.
 * 
 * @param signal  Contains the (2D) image data. Must be gray values
 *                and either of type float or double (currently, this MUST be double!)
 * 
 * @return True if successful, false otherwise.
 */
bool goRegionLS::setImage (goSignal3DBase<void>* signal, goDouble hx, goDouble hy)
{
    myPrivate->hx = hx;
    myPrivate->hy = hy;
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
        goDouble hx = myPrivate->hx;
        goDouble hy = myPrivate->hy;
        // initCircle (0.25f, hx, hy, myPrivate->phi);
        assert (myPrivate->phi.getDataType().getID() == GO_DOUBLE);
        initCircle<goDouble> (0.25f, hx, hy, myPrivate->phi);
        printf ("setImage(): phi max == %f, min == %f\n",myPrivate->phi.getMaximum(),myPrivate->phi.getMinimum());
    }
    return true;
}

/** 
 * @brief Sets the mu parameter.
 * 
 * @param mu  The value multiplied to the curvature term. Defaults to 1.
 */
void goRegionLS::setMu (goDouble mu)
{
    myPrivate->mu = mu;
}

/** 
 * @brief Sets the nu parameter.
 * 
 * @param nu  Area term. Defaults to 0.
 */
void goRegionLS::setNu (goDouble nu)
{
    myPrivate->nu = nu;
}

/** 
 * @brief Set multiplier for interior term.
 * 
 * @param l  Defaults to 1.
 */
void goRegionLS::setLambda1 (goDouble l)
{
    myPrivate->lambda1 = l;
}

/** 
 * @brief Set multiplier for exterior term.
 * 
 * @param l  Defaults to 1.
 */
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

/** 
 * @brief Approximation of Delingette's mean curvature regularisation for
 * parametric contours. As in the thesis of Picinbono, 1997.
 * 
 * @param d Factor for the term.
 */
void goRegionLS::setDelingette (goDouble d)
{
    myPrivate->delingetteFactor = d;
}

/** 
 * @brief Chunming Li's signed distance regularisation.
 *
 * This regulariser has a very strong effect.
 * 
 * @param li Factor for the term.
 */
void goRegionLS::setLi (goDouble li)
{
    myPrivate->liFactor = li;
}

//void goRegionLS::setHx (goDouble h)
//{
//    myPrivate->hx = h;
//}

//void goRegionLS::setHy (goDouble h)
//{
//    myPrivate->hy = h;
//}

/** 
 * @brief Get the current Phi grid.
 * 
 * @return Reference to the signal (2D) which contains the current Phi
 *         grid values. Should always be of type goDouble.
 */
goSignal3DBase<void>* goRegionLS::getPhi ()
{
    return &myPrivate->phi;
}

/** 
 * @brief Get time step restriction for explicit evolution.
 *
 * The time step is restricted and must meet the
 * CFL condition \f$ \Delta t \cdot \left( \frac{2\mu}{(\Delta x)^2} + \frac{2\mu}{(\Delta y)^2} \right) < 1 \f$. 
 * See e.g. the book of Osher and Fedkiw, "Level Set Methods and Dynamic
 * Implicit Surfaces"
 * 
 * @return \f$ \frac{1}{\left( \frac{2\mu}{(\Delta x)^2} + \frac{2\mu}{(\Delta y)^2} \right)} \f$.
 */
goDouble goRegionLS::getCFLRestriction()
{
    assert (myPrivate->hx != 0.0);
    assert (myPrivate->hy != 0.0);
    return 2*myPrivate->mu / (myPrivate->hx * myPrivate->hx)
         + 2*myPrivate->mu / (myPrivate->hy * myPrivate->hy);
}

/* 
 * @brief Regularised Heaviside function.
 * 
 * @param x 
 * @param epsilon 
 * 
 * @return 
 */
static goDouble heaviside (goDouble x, goDouble epsilon)
{
    static goDouble co = 2.0 / (goDouble)M_PI;
//    if (x < -epsilon)
//        return 0.0;
//    if (x > epsilon)
//        return 1.0;
    return 0.5*(1.0+co*atan(x/epsilon));
}

/* 
 * @brief Regularised dirac pulse.
 * 
 * @param x 
 * @param epsilon 
 * 
 * @return 
 */
static inline goDouble dirac (goDouble x, goDouble epsilon)
{
    return 0.3183098861837907/(epsilon*(1 + x*x/(epsilon*epsilon)));
}

template <class T,class phiT>
static inline void timesDiracPhi2 (goSignal3DBase<void>& f, const goSignal3DBase<void>& phi, goDouble epsilon)
{
    goSignal3DGenericIterator it(&f);
    goSignal3DGenericConstIterator itPhi(&phi);
    
    while (!it.endZ())
    {
        it.resetY();
        itPhi.resetY();
        while (!it.endY())
        {
            it.resetX();
            itPhi.resetX();
            while (!it.endX())
            {
                *(T*)*it *= dirac(*(phiT*)*itPhi,epsilon);
                it.incrementX();
                itPhi.incrementX();
            }
            it.incrementY();
            itPhi.incrementY();
        }
        it.incrementZ();
        itPhi.incrementZ();
    }
}

template <class T>
static inline void timesDiracPhi (goSignal3DBase<void>& f, const goSignal3DBase<void>& phi, goDouble epsilon)
{
    switch (phi.getDataType().getID())
    {
        case GO_FLOAT: timesDiracPhi2<T,goFloat> (f,phi,epsilon); break;
        case GO_DOUBLE: timesDiracPhi2<T,goDouble> (f,phi,epsilon); break;
        default: goLog::warning ("goRegionLS, timesDiracPhi(): only for float and double.");
                 break;
    }
}

template <class imageT, class paramT, class resultT, class phiT>
static inline void chanVeseImageTerm3 (goSignal3DBase<void>* image, goSignal3DBase<void>* result, goSignal3DBase<void>* phi, paramT c1, paramT c2, paramT lambda1, paramT lambda2)
{
    goSignal3DGenericIterator itImage (image);
    goSignal3DGenericIterator itResult (result);
    goSignal3DGenericIterator itPhi (phi);
    
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
                imageT imageValue = *(imageT*)*itImage;
                resultT l1 = imageValue - c1;
                l1 *= l1;
                resultT l2 = imageValue - c2;
                l2 *= l2;
                *(resultT*)*itResult += -lambda1 * l1 + lambda2 * l2;
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
}

template <class imageT, class paramT, class resultT>
static inline void chanVeseImageTerm2 (goSignal3DBase<void>* image, goSignal3DBase<void>* result, goSignal3DBase<void>* phi, paramT c1, paramT c2, paramT lambda1, paramT lambda2)
{
    switch (phi->getDataType().getID())
    {
        case GO_FLOAT:
            {
                chanVeseImageTerm3 <imageT,paramT,resultT,goFloat> (image, result, phi, c1, c2, lambda1, lambda2);
            }
            break;
        case GO_DOUBLE:
            {
                chanVeseImageTerm3 <imageT,paramT,resultT,goDouble> (image, result, phi, c1, c2, lambda1, lambda2);
            }
            break;
        default:
            {
                goLog::warning ("goRegionLS: chanVeseImageTerm2: unknown type for phi.");
            }
            break;
    }
}

template <class imageT, class paramT>
static inline void chanVeseImageTerm (goSignal3DBase<void>* image, goSignal3DBase<void>* result, goSignal3DBase<void>* phi, paramT c1, paramT c2, paramT lambda1, paramT lambda2)
{
    switch (result->getDataType().getID())
    {
        case GO_FLOAT:
            {
                chanVeseImageTerm2 <imageT,paramT,goFloat> (image, result, phi, c1, c2, lambda1, lambda2);
            }
            break;
        case GO_DOUBLE:
            {
                chanVeseImageTerm2 <imageT,paramT,goDouble> (image, result, phi, c1, c2, lambda1, lambda2);
            }
            break;
        default:
            {
                goLog::warning ("goRegionLS: chanVeseImageTerm: unknown type for result.");
            }
            break;
    }
}

/*
 * @brief Regularising term to push the level function \f$\Phi\f$ to signed distance functions.
 * 
 * Calculates \f$ \div \left[ (1 - \frac{1}{|\nabla \Phi|}) \cdot \nabla \Phi \right] \f$
 * using finite central differences.
 * 
 * From "Level Set Evolution Without Re-Initialization. A New Variational Approach."
 * by Chinming Li et al.
 * 
 * @note This is for clarity. The term could be calculated 
 * in the same loop with the curvature term, which would be faster.
 * 
 * @param input  Input \f$ \Phi \f$.
 * @param result Resulting values of the regulariser.
 * @param hx     Grid spacing in X.
 * @param hy     Grid spacing in Y.
 * 
 * @return True if successful, false otherwise.
 */
static inline bool liTerm (goSignal3DBase<void>& input, goSignal3D<void>& result, goDouble hx, goDouble hy)
{
    goSignal3D<void> nablaPhiX;
    nablaPhiX.setDataType (GO_FLOAT);
    goSignal3D<void> nablaPhiY;
    nablaPhiY.setDataType (GO_FLOAT);
    if (!goMath::centralDifferences(input, nablaPhiX, 0, hx))
        return false;
    if (!goMath::centralDifferences(input, nablaPhiY, 1, hy))
        return false;
    goSignal3DGenericIterator itX(&nablaPhiX);
    goSignal3DGenericIterator itY(&nablaPhiY);

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
                goFloat temp = sqrt(*(goFloat*)*itX * *(goFloat*)*itX + 
                                *(goFloat*)*itY * *(goFloat*)*itY);
                if (temp != 0.0)
                    temp = (1 - 1.0 / temp);
                *(goFloat*)*itX = *(goFloat*)*itX * temp;
                *(goFloat*)*itY = *(goFloat*)*itY * temp;
                itX.incrementX();
                itY.incrementX();
            }
            itX.incrementY();
            itY.incrementY();
        }
        itX.incrementZ();
        itY.incrementZ();
    }

    //= div(nablaPhi/|nablaPhi|)
    if (!goMath::divergence (nablaPhiX, nablaPhiY, hx, hy, result))
    {
        return false;
    }
    return true;
}

/*
 * @brief Curvature using goMath::centralDifferences() and goMath::divergence()
 *
 * Works for 2D. <br>
 * Calculates \f$\text{result} = div \frac{\nabla input}{|\nabla input)|}\f$
 * 
 * @param input 
 * @param result 
 * @param hx Horizontal grid spacing
 * @param hy Vertical grid spacing
 * 
 * @return True if successful, false oterhwise.
 */
static inline bool curvatureDivNabla (goSignal3DBase<void>& input, goSignal3D<void>& result, goDouble hx, goDouble hy)
{
    goSignal3D<void> nablaPhiX;
    nablaPhiX.setDataType (GO_DOUBLE);
    goSignal3D<void> nablaPhiY;
    nablaPhiY.setDataType (GO_DOUBLE);
    if (!goMath::centralDifferences(input, nablaPhiX, 0, hx))
        return false;
    if (!goMath::centralDifferences(input, nablaPhiY, 1, hy))
        return false;
    goSignal3DGenericIterator itX(&nablaPhiX);
    goSignal3DGenericIterator itY(&nablaPhiY);

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

    //= div(nablaPhi/|nablaPhi|)
    if (!goMath::divergence (nablaPhiX, nablaPhiY, hx, hy, result))
    {
        return false;
    }
    return true;
}

static inline bool curvatureDirect (goSignal3DBase<void>& input, goSignal3D<void>& result, goDouble hx, goDouble hy)
{
    if (input.getDataType().getID() != GO_DOUBLE)
    {
        goLog::warning ("curvatureDirect(): input must be double.");
        return false;
    }

    result.setDataType (GO_DOUBLE);
    result.make (&input);
    
    goSignal3DGenericConstIterator it1 (&input);
    goSignal3DGenericIterator it2 (&result);
    
    while (!it1.endY())
    {
        it1.resetX();
        it2.resetX();
        while (!it1.endX())
        {
            goDouble left      = *(const goDouble*)it1.leftX();
            goDouble p         = *(const goDouble*)*it1;
            goDouble right     = *(const goDouble*)it1.rightX();
            goDouble up        = *(const goDouble*)it1.leftY();
            goDouble down      = *(const goDouble*)it1.rightY();
            goDouble leftup    = *(const goDouble*)it1.leftUp();
            goDouble leftdown  = *(const goDouble*)it1.leftDown();
            goDouble rightup   = *(const goDouble*)it1.rightUp();
            goDouble rightdown = *(const goDouble*)it1.rightDown();
            goDouble phi_x     = 0.5 * (right - left) / hx;
            goDouble phi_y     = 0.5 * (down - up) / hy;
            goDouble phi_xy    = 0.25 * (leftup - leftdown - rightup + rightdown) / (hx*hy);
            goDouble phi_xx    = 0.25 * (left - 2*p + right) / (hx*hx);
            goDouble phi_yy    = 0.25 * (up - 2*p + down) / (hy*hy);
            goDouble denom = phi_x*phi_x + phi_y*phi_y;
            denom *= sqrt(denom);
            if (denom != 0.0)
            {
                *(goDouble*)*it2 = (phi_xx * phi_y * phi_y - 2.0f * phi_x * phi_y * phi_xy + phi_yy * phi_x * phi_x) / denom;
            }
            else
            {
                *(goDouble*)*it2 = 0.0;
            }

            it1.incrementX();
            it2.incrementX();
        }
        it1.incrementY();
        it2.incrementY();
    }
    return true;    
}

/** 
 * @brief Calculate the Chan&Vese term for current Phi and image.
 * 
 * Calculates dirac(Phi) * (mu*div*(nabla(Phi)/|nabla(Phi)| - nu - lambda1*(image - c1) + lambda2*(image - c2))), 
 * with c1 and c2 the mean inner and outer grey values of the image.
 * 
 * @param result  Must be of the image's size and of type goDouble. Contains the result for
 *                the above equation. Note that this is not yet multiplied by a time step.
 * 
 * @return True if successful, false otherwise.
 */
#define HAVE_MATLAB
#include <engine.h>
#include <gomatlab.h>
bool goRegionLS::chanVeseTerm (goSignal3DBase<void>& result)
{
    goDouble c2 = this->innerMean();
    goDouble c1 = this->outerMean();

    goSignal3D<void> curvTerm;
    if (myPrivate->mu != 0.0 || myPrivate->delingetteFactor != 0.0)
    {
        curvatureDivNabla (myPrivate->phi, curvTerm, myPrivate->hx, myPrivate->hy);
        // curvatureDirect (myPrivate->phi, curvTerm, myPrivate->hx, myPrivate->hy);
        
        //= Comparing curvatureDirect and curvatureDivNabla
#if 0
        goSignal3D<void> div;
        curvatureDivNabla (myPrivate->phi, div, myPrivate->hx, myPrivate->hy);
        goSignal3D<void> div2;
        curvatureDirect(myPrivate->phi, div2);
        static goMatlab matlab;
        matlab.putSignal (&div2,"div2");
        div2 -= div;
        GO_SIGNAL3D_EACHELEMENT_GENERIC(*(goDouble*)__ptr = fabs(*(goDouble*)__ptr), div2);
        goDouble max = div2.getMaximum();
        goDouble min = div2.getMinimum();
        goDouble mean = goSignalMean (div2);
        printf ("difference max == %f, min == %f, mean == %f\n",max,min,mean);
       
        matlab.putSignal (&div,"div");
        matlab.putSignal (&div2,"difference");

        matlab.matlabCall ("figure(10); imagesc(div); colormap (gray); colorbar; title('div');");
        matlab.matlabCall ("figure(11); imagesc(div2); colormap (gray); colorbar; title('div2'); drawnow;");
        matlab.matlabCall ("figure(12); imagesc(difference); colormap (gray); colorbar; title('difference'); waitforbuttonpress;");
#endif

#if 0
        myPrivate->matlab.signalToVariable (&nablaPhiX, "x");
        myPrivate->matlab.signalToVariable (&nablaPhiY, "y");
        myPrivate->matlab.signalToVariable (&div, "div");
        goString matlabResult;
        matlabResult.resize (1024);
        myPrivate->matlab.matlabCall ("div2 = divergence(x,y);\
                                       mean(mean(abs(div-div2))),\
                                       figure(1),imagesc(div2),colorbar;\
                                       figure(2),imagesc(div),colorbar;\
                                       waitforbuttonpress;", &matlabResult);
      
        printf ("%s\n",matlabResult.toCharPtr());
        goSignal3D<void> div2;
        if (!myPrivate->matlab.variableToSignal (&div2, "div2"))
        {
            goLog::error ("Could not get div from matlab.",this);
            return false;
        }
#endif 
        
        //= Now do div - nu - lambda1*(image - c1)^2 + lambda2*(image - c2)^2.
        if (!goCopySignal(&curvTerm,&result))
        {
            goLog::error ("chanVeseTerm(): could not copy div to result.",this);
            return false;
        }
        result *= myPrivate->mu;
    }
    else
    {
        switch (result.getDataType().getID())
        {
            case GO_FLOAT:
                {
                    goFloat value = 0.0;
                    result.fill (&value);
                }
                break;
            case GO_DOUBLE:
                {
                    goDouble value = 0.0;
                    result.fill (&value);
                }
                break;
            default: 
                goLog::warning("chanVeseTerm(): result data type must be float or double.",this); 
                return false; 
                break;
        }
    }

    //= Delingette/Picinbono:
    if (myPrivate->delingetteFactor != 0.0)
    {
        //= Filter curvature term to get the "Delingette-term" of mean curvature over a 5x5 neighbourhood (thesis Picinbono):
        goFilter3D<void,void> filter;
        static const goFloat mask [] = {1.0f, 1.0f, 1.0f, 1.0f, 1.0f,\
                                        1.0f, 1.0f, 1.0f, 1.0f, 1.0f,\
                                        1.0f, 1.0f, -24.0f, 1.0f, 1.0f,\
                                        1.0f, 1.0f, 1.0f, 1.0f, 1.0f,\
                                        1.0f, 1.0f, 1.0f, 1.0f, 1.0f};
        filter.setMask (mask,5,5,1,true,-1.0/25.0);
        filter.setMaskCenter (2,2,0);
        goSignal3D<void> div_temp;
        div_temp.setBorderFlags(GO_X|GO_Y,GO_CONSTANT_BORDER);
        div_temp.setDataType (curvTerm.getDataType().getID());
        div_temp.make (&curvTerm);
        filter.filter (curvTerm,div_temp);
        div_temp *= myPrivate->delingetteFactor;
        result += div_temp;
    }

    if (myPrivate->nu != 0.0)
    {
        result -= myPrivate->nu;
    }

    //= _Add_ the image term.
    switch (myPrivate->image->getDataType().getID())
    {
        case GO_FLOAT:
            {
                chanVeseImageTerm<goFloat,goDouble> (myPrivate->image, &result, &myPrivate->phi, c1, c2, myPrivate->lambda1, myPrivate->lambda2);
            }
            break;
        case GO_DOUBLE:
            {
                chanVeseImageTerm<goDouble,goDouble> (myPrivate->image, &result, &myPrivate->phi, c1, c2, myPrivate->lambda1, myPrivate->lambda2);
            }
            break;
        default:
            {
                goLog::error ("goRegionLS: currently, image MUST be goDouble or goFloat. Bailing out.", this);
                return false;
            }
            break;
    }
    return true;
}

/** 
* @brief Calculate \f$ \nabla \Phi \f$ with central differences.
*
* @see velocityFieldTimesGradPhi()
* 
* @param nablaPhi Contains result after the method returned true.
* 
* @return True if successful, false otherwise.
*/
bool goRegionLS::getNablaPhi (goSignal3D<void>& nablaPhi) const
{
    //= Some sanity checks.
    if (nablaPhi.getChannelCount() != 2)
    {
        goLog::warning("getNablaPhi(): nablaPhi must have 2 channels.",this);
        return false;
    }
    const goSignal3DBase<void>& phi = myPrivate->phi;
    if (nablaPhi.getSizeX() != phi.getSizeX() ||
        nablaPhi.getSizeY() != phi.getSizeY() ||
        nablaPhi.getSizeZ() != phi.getSizeZ())
    {
        goLog::warning("getNablaPhi(): nablaPhi must be of the same size as Phi.",this);
        return false;
    }

    nablaPhi.setChannel(0);
    bool ok = goMath::centralDifferences(phi,nablaPhi,0,myPrivate->hx);
    nablaPhi.setChannel(1);
    ok = ok && goMath::centralDifferences(phi,nablaPhi,1,myPrivate->hy);
    nablaPhi.setChannel(0);
    return ok;
}

template <class TV,class Tphi,class Tresult>
static bool upwindVTimesGradPhi4 (const goSignal3DBase<void>& V, 
                                  const goSignal3DBase<void>& phi,
                                  goDouble                    hx, 
                                  goDouble                    hy,
                                  goSignal3DBase<void>&       result)
{
    goSize_t tempChanV = V.getChannel();
    const_cast<goSignal3DBase<void>& >(V).setChannel(0);
    goSignal3DGenericConstIterator vIt(&V);
    const_cast<goSignal3DBase<void>& >(V).setChannel(tempChanV);
    goSignal3DGenericConstIterator it(&phi);
    goSignal3DGenericIterator resIt(&result);
    const goDouble hx_ = 1.0 / hx;
    const goDouble hy_ = 1.0 / hy;
    while (!it.endZ())
    {
        it.resetY();
        vIt.resetY();
        resIt.resetY();
        while (!it.endY())
        {
            it.resetX();
            vIt.resetX();
            resIt.resetX();
            while (!it.endX())
            {
                goDouble vx = *(const TV*)*vIt;
                goDouble vy = *((const TV*)*vIt + 1);
                goDouble gradX = 0.0;
                goDouble gradY = 0.0;
                Tphi central = *(const Tphi*)*it;  // saves one dereference and cast per point
                if (vx < 0.0)
                {
                    gradX = (*(const Tphi*)it.rightX() - central) * hx_;
                }
                else if (vx > 0.0)
                {
                    gradX = (central - *(const Tphi*)it.leftX()) * hx_;
                }
                if (vy < 0.0)
                {
                    gradY = (*(const Tphi*)it.rightY() - central) * hy_;
                }
                else if (vy > 0.0)
                {
                    gradY = (central - *(const Tphi*)it.leftY()) * hy_;
                }
                *(Tresult*)*resIt = vx * gradX + vy * gradY;
                it.incrementX();
                vIt.incrementX();
                resIt.incrementX();
            }
            it.incrementY();
            vIt.incrementY();
            resIt.incrementY();
        }
        it.incrementZ();
        vIt.incrementZ();
        resIt.incrementZ();
    }
    return true;
}
template <class TV,class Tphi>
static bool upwindVTimesGradPhi3 (const goSignal3DBase<void>& V, 
                                  const goSignal3DBase<void>& phi,
                                  goDouble                    hx,
                                  goDouble                    hy,
                                  goSignal3DBase<void>&       result)
{
    switch (result.getDataType().getID())
    {
        case GO_FLOAT:  return upwindVTimesGradPhi4<TV,Tphi,goFloat>  (V,phi,hx,hy,result); break;
        case GO_DOUBLE: return upwindVTimesGradPhi4<TV,Tphi,goDouble> (V,phi,hx,hy,result); break;
        default: goLog::warning("goRegionLS::velocityFieldTimesGradPhi(): bad type for phi.");
                 return false;
    }
}
template <class TV>
static bool upwindVTimesGradPhi2 (const goSignal3DBase<void>& V, 
                                  const goSignal3DBase<void>& phi,
                                  goDouble                    hx,
                                  goDouble                    hy,
                                  goSignal3DBase<void>&       result)
{
    switch (phi.getDataType().getID())
    {
        case GO_FLOAT:  return upwindVTimesGradPhi3<TV,goFloat>  (V,phi,hx,hy,result); break;
        case GO_DOUBLE: return upwindVTimesGradPhi3<TV,goDouble> (V,phi,hx,hy,result); break;
        default: goLog::warning("goRegionLS::velocityFieldTimesGradPhi(): bad type for phi.");
                 return false;
    }
}
/*
* @brief Calculate V * grad(phi) using the upwind finite difference scheme.
* 
* All signals can be of type GO_FLOAT or GO_DOUBLE, in any combination.
* Other types are not supported.
* 
* @param V 2D vector field
* @param phi phi
* @param hx x grid spacing
* @param hy y grid spacing
* @param result V * grad(phi)
* 
* @return True if successful, false otherwise.
*/
static bool upwindVTimesGradPhi (const goSignal3DBase<void>& V, 
                                 const goSignal3DBase<void>& phi,
                                 goDouble                    hx,
                                 goDouble                    hy,
                                 goSignal3DBase<void>&       result)
{
    if (V.getSize() != phi.getSize())
    {
        goLog::warning("goRegionLS::velocityFieldTimesGradPhi(): V and phi sizes mismatch");
        return false;
    }
    if (result.getSize() != phi.getSize())
    {
        goLog::warning("goRegionLS::velocityFieldTimesGradPhi(): result and phi sizes mismatch");
        return false;
    }
    if (V.getChannelCount() != 2)
    {
        goLog::warning("goRegionLS::velocityFieldTimesGradPhi(): V must be 2-channel data");
        return false;
    }
    switch (V.getDataType().getID())
    {
        case GO_FLOAT:  return upwindVTimesGradPhi2<goFloat>  (V,phi,hx,hy,result); break;
        case GO_DOUBLE: return upwindVTimesGradPhi2<goDouble> (V,phi,hx,hy,result); break;
        default: goLog::warning("goRegionLS::velocityFieldTimesGradPhi(): bad type for V.");
                 return false;
    }
}

/** 
* @brief Convenience function, multiplies V with nabla(Phi) using upwind scheme.
* 
* @param V 2D Vector field. Vector elements are stored in channels.
* @param result \f$ V \cdot \nabla\Phi \f$
* 
* @todo nabla phi is calculated various times during evolution.
*       Optimise that when it gets crucial.
* 
* @return True if successful, false otherwise. Check logfile.
*/
bool goRegionLS::velocityFieldTimesGradPhi (const goSignal3DBase<void>& V,
                                            goSignal3DBase<void>& result)
{
    const goSignal3DBase<void>& phi = myPrivate->phi;

#if 0
    //= Calculate grad(phi).
    goSignal3D<void> gradPhi;
    gradPhi.setDataType(GO_DOUBLE);
    gradPhi.setBorderFlags(GO_X|GO_Y, GO_CONSTANT_BORDER);
    //= 2-channel signal for a vector field.
    gradPhi.make(phi.getSizeX(), phi.getSizeY(), phi.getSizeZ(),
                 16, 16, phi.getBlockSizeZ(), 4, 4, phi.getBorderZ(), 2);
    if (!this->getNablaPhi (gradPhi))
    {
        return false;
    }
#endif

    //= NOTE:
    //= Calculate grad(phi) using upwind finite differences
    //= (use the direction from which the information flows to calculate
    //=  the gradient). Central differences would introduce errors.
    return upwindVTimesGradPhi (V,phi,myPrivate->hx,myPrivate->hy,result);
    
    //= Calculate V * grad(phi)
    // return goMath::vectorMult(V,gradPhi,result);
}

bool goRegionLS::timesDiracPhi (goSignal3DBase<void>& f)
{
    switch (f.getDataType().getID())
    {
        case GO_FLOAT: ::timesDiracPhi<goFloat> (f,myPrivate->phi,myPrivate->epsilon); break;
        case GO_DOUBLE: ::timesDiracPhi<goDouble> (f,myPrivate->phi,myPrivate->epsilon); break;
        default: goLog::warning("timesDirac(): f must be float or double.",this); return false; break;
    }
    return true;
}

/** 
 * @brief Evolves one time step.
 * 
 * @param deltaT Time step. Note that there is a restriction on the step since
 *               the PDE is solved explicitly.
 * 
 * @return True if successful, false otherwise.
 */
bool goRegionLS::evolve (goDouble deltaT, const goSignal3DBase<void>* externalVelocity, goDouble externalFactor)
{
    //= delta_phi = chan-vese-term;
    //= phi = phi + deltaT * delta_phi;
    //= dirac_epsilon(phi) * (div(nablaPhi/|nablaPhi|) - nu - 
    //=                                 lambda1 * (image - c1)^2 + 
    //=                                 lambda2 * (image - c2)^2)
    
    goSignal3D<void> chanVese;
    chanVese.setDataType(myPrivate->image->getDataType().getID());
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
    if (myPrivate->liFactor != 0.0)
    {
        goSignal3D<void> li;
        li.setDataType(myPrivate->image->getDataType().getID());
        li.make (i->getSizeX(), i->getSizeY(), i->getSizeZ(),
                i->getBlockSizeX(), i->getBlockSizeY(), i->getBlockSizeZ(),
                4,4,4);
        liTerm (myPrivate->phi, li, myPrivate->hx, myPrivate->hy);
        li *= myPrivate->liFactor;
        chanVese += li;
    }
    if (externalVelocity)
    {
        //= Make a scalar field of size(Phi)
        goSignal3D<void> v_times_grad_phi;
        v_times_grad_phi.setDataType(GO_DOUBLE);
        v_times_grad_phi.make(&myPrivate->phi);
        if (!this->velocityFieldTimesGradPhi(*externalVelocity, v_times_grad_phi))
        {
            goLog::warning("evolve(): velocityFieldTimesGradPhi() failed.",this);
        }
        else
        {
            v_times_grad_phi *= externalFactor;
            chanVese -= v_times_grad_phi;
        }
    }
    
    //= Everything times dirac(Phi).
    this->timesDiracPhi (chanVese);
    
    //= Everything times delta_t.
    chanVese *= deltaT;
    myPrivate->phi += chanVese;
    
    return true;
}

//#define HAVE_MATLAB
//#include "mex.h"
//#include "engine.h"
//#include <gomatlab.h>

/** 
 * @brief Implicit evolution step.
 *
 * @note The main part of computation time seems to go to solving
 * the resulting equation system.
 * 
 * @param deltaT Time step.
 * 
 * @return True if successful, false otherwise.
 */
bool goRegionLS::evolveImplicitly (goDouble deltaT)
{
    // static goMatlab matlab;
    goSignal3DBase<void>* i = myPrivate->image;
    assert(i);

    if (!i)
    {
        goLog::warning ("evolveImplicitly(): image is not set.",this);
        return false;
    }
    
    if (myPrivate->image->getDataType().getID() != GO_DOUBLE)
    {
        goLog::warning ("evolveImplicitly(): image type must currently be double.",this);
        return false;
    }
    
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
    goDouble hx_2 = 1.0 / (myPrivate->hx * myPrivate->hx);
    goDouble hy_2 = 1.0 / (myPrivate->hy * myPrivate->hy);
    while(!itPhi.endY())
    {
        itPhi.resetX();
        itImage.resetX();
        goIndex_t x = 0;

        //= ============== Was tun am Rand? ===============
        
        while (!itPhi.endX())
        {
            goDouble diracPhi = dirac(*(goDouble*)*itPhi,myPrivate->epsilon);
            //= temp1 = Delta_+^x(Phi^n_{i,j})^2 / h_x^2
            goDouble temp1 = *(goDouble*)itPhi.rightX() - *(goDouble*)*itPhi;
            temp1 = temp1 * temp1 * hx_2; // temp1*temp1 / (myPrivate->hx*myPrivate->hx);
            //= temp2 = (Phi^n_{i,j+1} - Phi^n_{i,j-1})^2 / (2h_y)^2
            goDouble temp2 = *(goDouble*)itPhi.rightY() - *(goDouble*)itPhi.leftY();
            temp2 = temp2 * temp2 * 0.25 * hy_2; // temp2*temp2 / (4*myPrivate->hy*myPrivate->hy);
            goDouble mu_x = 0.0;
            if (temp1 + temp2 != 0.0)
            {
                mu_x = myPrivate->mu * hx_2
                    * deltaT
                    / sqrt(temp1 + temp2) * diracPhi;
//                mu_x = myPrivate->mu / (myPrivate->hx*myPrivate->hx)
//                    * deltaT
//                    / sqrt(temp1 + temp2) * diracPhi;
            }

            temp1 = *(goDouble*)itPhi.rightX() - *(goDouble*)itPhi.leftX();
            temp1 = temp1 * temp1 * 0.25 * hx_2; // temp1*temp1 / (4*myPrivate->hx*myPrivate->hx);
            temp2 = *(goDouble*)itPhi.rightY() - *(goDouble*)*itPhi;
            temp2 = temp2 * temp2 * hy_2; // temp2*temp2 / (myPrivate->hy*myPrivate->hy);
            goDouble mu_y = 0.0;
            if (temp1 + temp2 != 0.0)
            {
                mu_y = myPrivate->mu * hy_2 /* / (myPrivate->hy*myPrivate->hy) */
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
    goTimerObject timer;
    printf ("goRegionLS: solving %d x %d system with %d non-zero entries.\n", A.getRowCount(), A.getColumnCount(), A.getElementCount());
    timer.startTimer();
    if (goMath::goConjugateGradients(A,b,newPhi,1e-6) > 1e-6)
    {
        goLog::warning("goConjugateGradients() failed.",this);
        return false;
    }
    timer.stopTimer();
    printf ("   goConjugateGradients() took %f seconds.\n", timer.getTimerSeconds());

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

/** 
 * @brief Calculate the inner mean.
 * 
 * @return Inner mean (where Phi > 0)
 */
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

/** 
 * @brief Calculate the outer mean.
 * 
 * @return Outer mean (where Phi < 0)
 */
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
