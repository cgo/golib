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

class goRegionLSPrivate;

/** @addtogroup levelsets Level set methods
 * @{
 */
/** --------------------------------------------------------------------------
 * @brief Region based level set segmentation (after Chan and Vese).
 *
 * This is an implementation of region based active contours in 2D.
 * Be aware that the PDE involved is solved the explicit way, not implicitly,
 * so there are restrictions on the time step. See e.g. Osher&Fedkiw for details.
 ----------------------------------------------------------------------------*/
class goRegionLS : public goObjectBase
{
    public:
        goRegionLS ();
        virtual ~goRegionLS ();

        bool setImage   (goSignal3DBase<void>* signal);
        void setMu      (goDouble mu);
        void setNu      (goDouble nu);
        void setLambda1 (goDouble l1);
        void setLambda2 (goDouble l2);
        void setEpsilon (goDouble e);

        //= Special regularising terms.
        void setDelingette (goDouble d);
        void setLi         (goDouble li);

        void setHx      (goDouble h);
        void setHy      (goDouble h);
        
        goSignal3DBase<void>* getPhi ();
        
        bool evolve           (goDouble deltaT);
        bool evolveImplicitly (goDouble deltaT);

        goDouble innerMean ();
        goDouble outerMean ();

        bool chanVeseTerm (goSignal3DBase<void>& result);

        bool timesDiracPhi (goSignal3DBase<void>&);
    private:
        goRegionLSPrivate* myPrivate;
};
/** @} */

#endif
