#ifndef GOPOSITION_H
#define GOPOSITION_H

#include <go3vector.h>

/**
 * @brief 3D position.
 *
 * Indices are goInt32.
 **/
class
goPosition : public go3Vector<goInt32> {
 public:
  goPosition ();
  goPosition (goInt32 x, goInt32 y, goInt32 z);
  virtual ~goPosition ();

 protected:
};

#endif
