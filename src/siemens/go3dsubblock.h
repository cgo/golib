#ifndef __GO3DSUBBLOCK_H__
#define __GO3DSUBBLOCK_H__

#include <go3dblock.h>
#include <gotypes.h>
#include <goposition.h>

/*!
 * Enables access to subblocks of a given <CODE>go3DBlock</CODE>.
 * Being derived from it, <CODE>go3DSubBlock</CODE> can do anything a 
 * <CODE>go3DBlock</CODE> can.
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
go3DSubBlock : public go3DBlock<T> {
 public:
  go3DSubBlock ();
  go3DSubBlock (go3DBlock<T>*);
  virtual ~go3DSubBlock();

  void setParent (go3DBlock<T> *p);
  go3DBlock<T> *getParent () { return parent; }
  void setPosition (goIndex_t x, goIndex_t y, goIndex_t z);
  /*!
   * Sets the position of the subblock in the parent block.
   * Take care, this uses internal pointer differences for the parent
   * which get invalid if you change the diffs in the parent.
   */
  void setPosition (goPosition &p);
  goPosition& getPosition () { return position; }

  void move (int dir);

 protected:
  goPosition position;
  go3DBlock<T> *parent;
  goPtrdiff_t parentXDiff;
  goPtrdiff_t parentYDiff;
  goPtrdiff_t parentZDiff;
};

#endif
