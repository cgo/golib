#ifndef GO4VECTOR_H
#define GO4VECTOR_H

#include <gotypes.h>
#include <go44matrix.h>

#include <math.h>
#include <ostream.h>

#define GO4VECTOR_FUNCTION_PREFIX inline
// #define GO4VECTOR_FUNCTION_PREFIX

// template <class T> class go44Matrix;

template< class T >
class
go4Vector {
 public:
  go4Vector ();
  virtual ~go4Vector ();
  
  // GO4VECTOR_FUNCTION_PREFIX T& operator[] (goIndex_t i) { return data[i]; }
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator= (go4Vector<T>& other) 
  { x = other.x; 
    y = other.y; 
    z = other.z; 
    t = other.t; 
    return *this; }

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator= (go3Vector<T>& other) 
  { x = other.x; 
    y = other.y; 
    z = other.z; 
    t = 1; 
    return *this; }
  
  // const T& operator[] (goIndex_t i) const { return data[i]; }

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator-= (go4Vector<T>& other)
  {
    x -= other.x;
    y -= other.y;
    z -= other.z;
    t -= other.t;
    return *this;
  }
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator-= (const go4Vector<T>& other)
  {
    x -= other.x;
    y -= other.y;
    z -= other.z;
    t -= other.t;
    return *this;
  }
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator+= (go4Vector<T>& other)
  {
    x += other.x;
    y += other.y;
    z += other.z;
    t += other.t;
    return *this;
  }
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator*= (T other)
  {
    x *= other;
    y *= other;
    z *= other;
    t *= other;
    return *this;
  }

  GO4VECTOR_FUNCTION_PREFIX T operator* (go4Vector<T>& other) {
    return (other.x * x + other.y * y + other.z * z + other.t * t);
  }

  GO4VECTOR_FUNCTION_PREFIX T operator* (const go4Vector<T>& other) const {
    return (other.x * x + other.y * y + other.z * z + other.t * t);
  }

  /*!
   * Multiplies matrix to this vector from the left (naturally), i.e.
   * sets *this = matrix * *this;
   */
  GO4VECTOR_FUNCTION_PREFIX void operator*= (go44Matrix<T>& matrix)
  {
    T a1,a2,a3,a4;
    T* m = matrix.getPtr();
    a1 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
    m += 4;
    a2 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
    m += 4;
    a3 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
    m += 4;
    a4 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
/*      a1 = *(m) * x + *(++m) * y + *(++m) * z + *(++m) * t; */
/*      a2 = *(++m) * x + *(++m) * y + *(++m) * z + *(++m) * t; */
/*      a3 = *(++m) * x + *(++m) * y + *(++m) * z + *(++m) * t; */
/*      a4 = *(++m) * x + *(++m) * y + *(++m) * z + *(++m) * t; */
    x = a1; y = a2; z = a3; t = a4;
  }

  /* Not possible. Maybe just use x,y,z and forget about t. */  
/*    GO4VECTOR_FUNCTION_PREFIX void cross (go4Vector<T>& other) { */
/*      T x0,x1,x2; */
/*      x0 = y * other.z - z * other.y; */
/*      x1 = z * other.x - other.z * x; */
/*      x2 = x * other.y - other.x * y; */
/*      x = x0; */
/*      y = x1; */
/*      z = x2; */
/*    } */

  GO4VECTOR_FUNCTION_PREFIX goDouble abs() 
    {
      return (goDouble)sqrt( (x * x) + 
			     (y * y) + 
			     (z * z) +
			     (t * t) );
    }

  /*!
   * Divides everything by t (for homog. coordinates).
   */
  GO4VECTOR_FUNCTION_PREFIX void div() 
    {
      T t_1 = (T)(1 / (goDouble)t);
      t = 1;
      x *= t_1;
      y *= t_1;
      z *= t_1;
    }

  friend ostream& operator<< (ostream &o, go4Vector<goDouble>& v);
  friend ostream& operator<< (ostream &o, go4Vector<goFloat>& v);

  T x, y, z, t;

 protected:

};



#endif




