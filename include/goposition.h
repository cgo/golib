#ifndef GOPOSITION_H
#define GOPOSITION_H

#include <go3vector.h>

class
goPosition : public go3Vector<goInt32> {
 public:
  goPosition ();
  goPosition (goInt32, goInt32, goInt32);
  virtual ~goPosition ();

 protected:
};

#endif
