#include <goposition.h>

goPosition::goPosition ()
  : go3Vector<goInt32> () {
}

goPosition::goPosition (goInt32 x, goInt32 y, goInt32 z)
    : go3Vector<goInt32> (x, y, z)
{
}

goPosition::~goPosition () {
}

