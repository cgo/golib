#ifndef __GOSUBSIGNAL3D_H__
#define __GOSUBSIGNAL3D_H__

#include <gosignal3d.h>
#include <gotypes.h>
#include <goposition.h>

/** \addtogroup signal 
 * @{ */
/*! \brief Access to sub-areas of <code>goSignal3DBase</code> objects
 *
 * Enables access to sub signals of a given <CODE>goSignal3DBase</CODE>.
 * Being derived from it, <CODE>goSubSignal3D</CODE> can do anything a 
 * <CODE>goSignal3D</CODE> can.
 * A parent must be set and the position is initialised to (0,0,0).
 * <CODE>move()</CODE> moves in one direction of <br>
 * <ul>
 *  <li>GO_DIRECTION_X</li>
 *  <li>GO_DIRECTION_Y</li>
 *  <li>GO_DIRECTION_Z</li>
 *  <li>GO_DIRECTION_X_NEGATIVE</li>
 *  <li>GO_DIRECTION_Y_NEGATIVE</li>
 *  <li>GO_DIRECTION_Z_NEGATIVE</li>
 * </ul>
 */
template< class T >
class
goSubSignal3D : public goSignal3DBase<T> 
{
    public:
        goSubSignal3D ();
        goSubSignal3D (goSignal3DBase<T>* p, 
                       goSize_t           sizeX,
                       goSize_t           sizeY,
                       goSize_t           sizeZ);
        virtual ~goSubSignal3D();

        void               setParent (goSignal3DBase<T>* p);
        goSignal3DBase<T>* getParent () { return parent; }
        /*!
         * Sets the position of the subblock in the parent block.
         * Take care, this uses internal pointer differences for the parent
         * which get invalid if you change the diffs in the parent.
         */
        void setPosition (goPosition &p);
        void setPosition (goIndex_t x, goIndex_t y, goIndex_t z);

        void setSkip (goSize_t skipX, goSize_t skipY, goSize_t skipZ);
        
        goPosition& getPosition () { return position; }

        void move (int dir);

        void shiftLeftSize  (int n);
        void shiftRightSize (int n);
        void shiftLeftDiff  (int n);
        void shiftRightDiff (int n);
        
    private:
        goPosition         position;
        goSignal3DBase<T>* parent;
        goSize_t           skipX;
        goSize_t           skipY;
        goSize_t           skipZ;
        bool               deleteX;
        bool               deleteY;
        bool               deleteZ;
};
/** @} */

#endif




