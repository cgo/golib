#ifndef __GOSUBSIGNAL3D_H__
#define __GOSUBSIGNAL3D_H__

#include <gosignal3d.h>
#include <gotypes.h>
#include <goposition.h>

/*!
 * Enables access to sub signals of a given <CODE>goSignal3D</CODE>.
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
goSubSignal3D : public goSignal3D<T> {
 public:
  goSubSignal3D ();
  goSubSignal3D (goSignal3D<T>*);
  virtual ~goSubSignal3D();

  void setParent (goSignal3D<T> *p);
  goSignal3D<T> *getParent () { return parent; }
  /*!
   * Sets the position of the subblock in the parent block.
   * Take care, this uses internal pointer differences for the parent
   * which get invalid if you change the diffs in the parent.
   */
  void setPosition (goPosition &p);
  void setPosition (int x, int y, int z);
  /*!
   * Sets the position using the pointer differences of this subsignal instead
   * of those belonging to the parent signal.
   */
  void setPositionSub (int x, int y, int z);

  goPosition& getPosition () { return position; }

  void move (int dir);

 protected:
  goPosition position;
  goSignal3D<T> *parent;
  goPtrdiff_t parentXDiff;
  goPtrdiff_t parentYDiff;
  goPtrdiff_t parentZDiff;
};

#endif




