#ifndef GO3DINFO_H
#define GO3DINFO_H

#include <gotypes.h>
#include <go3vector.h>

class
go3DInfo {
 public:
  go3DInfo () 
    {
      position.x = 0;
      position.y = 0;
      position.z = 0;
      size.x = 1;
      size.y = 1;
      size.z = 1;
    }
  virtual ~go3DInfo () { }

  go3DInfo& operator= (go3DInfo& other) {
    position = other.position;
    size = other.size;
    return *this;
  }

  go3Vector<goDouble>	position;
  go3Vector<goDouble>	size;
  
};


#endif
