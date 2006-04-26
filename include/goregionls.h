#ifndef GOREGIONLS_H
#define GOREGIONLS_H

#ifndef GOOBJECTBASE_H
# include <goobjectbase.h>
#endif
#ifndef GOTYPES_H
# include <gotypes.h>
#endif
#ifndef GOSIGNAL3DBASE_H
# include <gosignal3dbase.h>
#endif
#ifndef GOSIGNAL3D_H
# include <gosignal3d.h>
#endif

class goRegionLSPrivate;

/** @addtogroup levelsets Level set methods
 * @{
 */
/** 
 * @brief Region based level set segmentation (after Chan and Vese).
 *
 * This is an implementation of region based active contours in 2D.
 * Be aware that the PDE involved is solved the explicit way, not implicitly,
 * so there are restrictions on the time step. See e.g. Osher&Fedkiw for details.
 */
class goRegionLS : public goObjectBase
{
    public:
        goRegionLS ();
        virtual ~goRegionLS ();

        bool setImage   (goSignal3DBase<void>* signal, goDouble hx, goDouble hy);
        void setMu      (goDouble mu);
        void setNu      (goDouble nu);
        void setLambda1 (goDouble l1);
        void setLambda2 (goDouble l2);
        void setEpsilon (goDouble e);

        //= Special regularising terms.
        void setDelingette (goDouble d);
        void setLi         (goDouble li);

        goSignal3DBase<void>* getPhi ();

        goDouble getCFLRestriction();
        
        bool evolve           (goDouble timeStep, 
                               const goSignal3DBase<void>* externalVelocity = 0, 
                               goDouble externalFactor = 1.0);
        bool evolveImplicitly (goDouble timeStep);

        goDouble innerMean ();
        goDouble outerMean ();

        bool chanVeseTerm  (goSignal3DBase<void>& result);
        bool getNablaPhi   (goSignal3D<void>& nablaPhi) const;
        bool velocityFieldTimesGradPhi (const goSignal3DBase<void>& V,
                                        goSignal3DBase<void>& result);

        bool timesDiracPhi (goSignal3DBase<void>& f);
    private:
        goRegionLSPrivate* myPrivate;
};
/** @} */

#endif
