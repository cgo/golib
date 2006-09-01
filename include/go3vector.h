#ifndef GO3VECTOR_H
#define GO3VECTOR_H

#ifndef GOTYPES_H
# define GO3VECTOR_H_INSIDE
# include <gotypes.h>
# undef GO3VECTOR_H_INSIDE
#endif
#include <math.h>
#include <iostream>

#define GO3VECTOR_FUNCTION_PREFIX inline
// #define GO3VECTOR_FUNCTION_PREFIX

/*!
 * \addtogroup math
 * @{
 */
/**
 * @brief 3-dimensional vector.
 **/
template< class T >
class
go3Vector {
 public:
  go3Vector ()
     : x (T(0)), y (T(0)), z (T(0)) 
  {};
  go3Vector (const go3Vector<T>& other)
      : x (other.x), y (other.y), z (other.z)
  {};
  go3Vector (T t1, T t2, T t3)
      : x(t1), y(t2), z(t3)
  {};
  virtual ~go3Vector () {};
 
  template <class To> bool operator== (const go3Vector<To>& other) const
  {
      if (x == static_cast<T>(other.x) &&
          y == static_cast<T>(other.y) &&
          z == static_cast<T>(other.z))
      {
          return true;
      }
      return false;
  }
  template <class To> bool operator!= (const go3Vector<To>& other) const
  {
      return !this->operator==(other);
  }
  // GO3VECTOR_FUNCTION_PREFIX T& operator[] (goIndex_t i) { return data[i]; }
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T>& operator= (const go3Vector<T> other) 
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

  GO3VECTOR_FUNCTION_PREFIX go3Vector<T> operator* (T val)
  {
	  go3Vector<T> tempV;
	  tempV.x = x * val;
	  tempV.y = y * val;
	  tempV.z = z * val;
	  return tempV;
  }

  GO3VECTOR_FUNCTION_PREFIX go3Vector<T> operator* (const T val) const
  {
	  go3Vector<T> tempV;
	  tempV.x = x * val;
	  tempV.y = y * val;
	  tempV.z = z * val;
	  return tempV;
  }
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T> operator+ (go3Vector<T>& other)
  {
	  go3Vector<T> tempV;
	  tempV.x = x + other.x;
	  tempV.y = y + other.y;
	  tempV.z = z + other.z;
	  return tempV;
  }
  
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T> operator+ (go3Vector<T>& other) const
  {
	  go3Vector<T> tempV;
	  tempV.x = x + other.x;
	  tempV.y = y + other.y;
	  tempV.z = z + other.z;
	  return tempV;
  }
  GO3VECTOR_FUNCTION_PREFIX go3Vector<T> operator+ (const go3Vector<T>& other) const
  {
	  go3Vector<T> tempV;
	  tempV.x = x + other.x;
	  tempV.y = y + other.y;
	  tempV.z = z + other.z;
	  return tempV;
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

//  friend ostream& operator<< (ostream &o, go3Vector<goDouble>& v);
//  friend ostream& operator<< (ostream &o, go3Vector<goFloat>& v);

  T x, y, z;

 protected:

};

/*! @} */


#endif




