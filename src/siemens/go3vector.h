#ifndef GO3VECTOR_H
#define GO3VECTOR_H

#include <gotypes.h>
#include <math.h>
#include <ostream.h>

#define GO3VECTOR_FUNCTION_PREFIX inline
// #define GO3VECTOR_FUNCTION_PREFIX


template< class T >
class
go3Vector {
 public:
  go3Vector ();
  virtual ~go3Vector ();
  
  // GO3VECTOR_FUNCTION_PREFIX T& operator[] (goIndex_t i) { return data[i]; }
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T>& operator= (go3Vector<T>& other) 
  { x = other.x; y = other.y; z = other.z; return *this; }
  
  // const T& operator[] (goIndex_t i) const { return data[i]; }

  GO3VECTOR_FUNCTION_PREFIX go3Vector<T>& operator-= (go3Vector<T>& other)
  {
    x -= other.x;
    y -= other.y;
    z -= other.z;
    return *this;
  }
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T>& operator-= (const go3Vector<T>& other)
  {
    x -= other.x;
    y -= other.y;
    z -= other.z;
    return *this;
  }
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T>& operator+= (go3Vector<T>& other)
  {
    x += other.x;
    y += other.y;
    z += other.z;
    return *this;
  }
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T>& operator*= (T other)
  {
    x *= other;
    y *= other;
    z *= other;
    return *this;
  }

  GO3VECTOR_FUNCTION_PREFIX T operator* (go3Vector<T>& other) {
    return (other.x * x + other.y * y + other.z * z);
  }

  GO3VECTOR_FUNCTION_PREFIX T operator* (const go3Vector<T>& other) const {
    return (other.x * x + other.y * y + other.z * z);
  }
  
  GO3VECTOR_FUNCTION_PREFIX void cross (go3Vector<T>& other) {
    T x0,x1,x2;
    x0 = y * other.z - z * other.y;
    x1 = z * other.x - other.z * x;
    x2 = x * other.y - other.x * y;
    x = x0;
    y = x1;
    z = x2;
  }

  GO3VECTOR_FUNCTION_PREFIX goDouble abs() 
    {
      return (goDouble)sqrt((x * x) + 
			    (y * y) + 
			    (z * z));
    }

  friend ostream& operator<< (ostream &o, go3Vector<goDouble>& v);

  T x, y, z;

 protected:

};



#endif




