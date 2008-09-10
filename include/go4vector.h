#ifndef GO4VECTOR_H
#define GO4VECTOR_H

#include <gotypes.h>
#include <go44matrix.h>
#include <go3vector.h>
#include <golog.h>
#include <math.h>
#include <ostream>

#define GO4VECTOR_FUNCTION_PREFIX inline
// #define GO4VECTOR_FUNCTION_PREFIX

// template <class T> class go44Matrix;

//= New version of go4Vector -- remplase old when this is done.
#if 1
template <class T>
class go4Vector : public goMath::Vector<T>
{
    public:
        go4Vector (T x_ = (T)0, T y_ = (T)0, T z_ = (T)0, T t_ = (T)0) 
            : goMath::Vector<T>(4) 
        {
            (*this)[0] = x_;
            (*this)[1] = y_;
            (*this)[2] = z_;
            (*this)[3] = t_;
        };
        template <class To>
        go4Vector (const go4Vector<To>& other)
            : goMath::Vector<T>(4)
        {
            *this = other;
        };

        template <class To>
        go4Vector (const goMath::Vector<To>& other)
            : goMath::Vector<T>(4)
        {
            if (other.getSize() == 4)
            {
                *this = other;
            }
            else
            {
                goLog::warning ("go4Vector::go4Vector(other): Tried to initialise with goMath::Vector of size != 4");
            }
        };
        virtual ~go4Vector () {};

        T& x() { return (*this)[0]; };
        T& y() { return (*this)[1]; };
        T& z() { return (*this)[2]; };
        T& t() { return (*this)[3]; };
        T& w() { return (*this)[3]; };
        const T& x() const { return (*this)[0]; };
        const T& y() const { return (*this)[1]; };
        const T& z() const { return (*this)[2]; };
        const T& t() const { return (*this)[3]; };
        const T& w() const { return (*this)[3]; };

#if 0
        template <class To>
            GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator= (const go4Vector<To>& other) 
            {   (*this)[0] = other.x; 
                (*this)[1] = other.y; 
                (*this)[2] = other.z; 
                (*this)[3] = 1; 
                return *this; };
#endif
#if 0
      /*!
       * Multiplies matrix to this vector from the left (naturally), i.e.
       * sets *this = matrix * *this;
       */
      template <class mT>
      GO4VECTOR_FUNCTION_PREFIX void operator*= (const go44Matrix<mT>& matrix)
      {
        T a1,a2,a3,a4;
        const mT* m = matrix.getPtr();
        a1 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
        m += 4;
        a2 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
        m += 4;
        a3 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
        m += 4;
        a4 = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
        x = a1; y = a2; z = a3; t = a4;
      };
      template <class mT>
          GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator* (const go44Matrix<mT>& matrix) const
          {
              go4Vector<T> retValue;
              const mT* m = matrix.getPtr();
              retValue.x = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
              m += 4;
              retValue.y = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
              m += 4;
              retValue.z = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
              m += 4;
              retValue.w = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
              return retValue;
          };
#endif
      /*! @brief Cross-product.
       * Uses only the first three elements. */
      GO4VECTOR_FUNCTION_PREFIX void cross (const go4Vector<T>& other) 
      { 
          T x0,x1,x2; 
          x0 = y() * other.z() - z() * other.y(); 
          x1 = z() * other.x() - other.z() * x(); 
          x2 = x() * other.y() - other.x() * y(); 
          x() = x0; 
          y() = x1; 
          z() = x2; 
      };

      /*!
       * Divides everything by t (for homog. coordinates).
       */
      GO4VECTOR_FUNCTION_PREFIX void div() 
      {
          T t_1 = (T)(1.0 / (goDouble)t());
          t() = 1;
          x() *= t_1;
          y() *= t_1;
          z() *= t_1;
      };
};
#endif
/*! \addtogroup math
 * @{
 */
/**
 * @brief 4-dimensional vector.
 * @todo make this a descendant of goMath::Vector, fix size to 4, provide
 *  x,y,z,t/w as references.
 **/
#if 0
template< class T >
class
go4Vector {
 public:
     /** 
      * @brief Constructor.
      * 
      * @param x_ 1st entry. Default 0.
      * @param y_ 2nd entry. Default 0.
      * @param z_ 3rd entry. Default 0.
      * @param t_ 4th entry. Default 0.
      */
  go4Vector (T x_ = (T)0, T y_ = (T)0, T z_ = (T)0, T t_ = (T)0) : x(x_), y(y_), z(z_), t(t_) { };

  template <class To>
  go4Vector (const go4Vector<To>& other) { *this = other; };

  virtual ~go4Vector () {};
 
  GO4VECTOR_FUNCTION_PREFIX bool operator== (const go4Vector<T>& other) const
  {
      if (x == other.x &&
          y == other.y &&
          z == other.z &&
          t == other.t)
      {
          return true;
      }
      return false;
  };

  GO4VECTOR_FUNCTION_PREFIX bool operator!= (const go4Vector<T>& other) const
  {
      return !(*this == other);
  };
  
  template <class To>
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator= (const go4Vector<To>& other) 
  { x = other.x; 
    y = other.y; 
    z = other.z; 
    t = other.t; 
    return *this; };

  template <class To>
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator= (const go3Vector<To>& other) 
  { x = other.x; 
    y = other.y; 
    z = other.z; 
    t = 1; 
    return *this; };
  
  // const T& operator[] (goIndex_t i) const { return data[i]; }

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator-= (const go4Vector<T>& other)
  {
    x -= other.x;
    y -= other.y;
    z -= other.z;
    t -= other.t;
    return *this;
  };

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator+= (const go4Vector<T>& other)
  {
    x += other.x;
    y += other.y;
    z += other.z;
    t += other.t;
    return *this;
  };

  /*!
   * Multiplies matrix to this vector from the left (naturally), i.e.
   * sets *this = matrix * *this;
   */
  template <class mT>
  GO4VECTOR_FUNCTION_PREFIX void operator*= (const go44Matrix<mT>& matrix)
  {
    T a1,a2,a3,a4;
    const mT* m = matrix.getPtr();
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
  };

  template <class To>
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator*= (To other)
  {
    x *= other;
    y *= other;
    z *= other;
    t *= other;
    return *this;
  };

  template <class To>
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T>& operator/= (To other)
  {
      To frac = To(1.0 / other);
      x *= frac;
      y *= frac;
      z *= frac;
      t *= frac;
      return *this;
  };

  GO4VECTOR_FUNCTION_PREFIX T operator* (const go4Vector<T>& other) const {
    return (other.x * x + other.y * y + other.z * z + other.t * t);
  };

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator+ (const go4Vector<T>& other) const
  {
      go4Vector<T> ret;
      ret.x = this->x + other.x;
      ret.y = this->y + other.y;
      ret.z = this->z + other.z;
      ret.t = this->t + other.t;
      return ret;
  };

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator- (const go4Vector<T>& other) const
  {
      go4Vector<T> ret;
      ret.x = this->x - other.x;
      ret.y = this->y - other.y;
      ret.z = this->z - other.z;
      ret.t = this->t - other.t;
      return ret;
  };

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator* (goDouble n) const
  {
    return go4Vector<T> (T(this->x * n), T(this->y * n), T(this->z * n), T(this->t * n));
  };

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator/ (goDouble n) const
  {
    return go4Vector<T> (T(this->x / n), T(this->y / n), T(this->z / n), T(this->t / n));
  };

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator+ (goDouble n) const
  {
    return go4Vector<T> (T(this->x + n), T(this->y + n), T(this->z + n), T(this->t + n));
  };

  GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator- (goDouble n) const
  {
    return go4Vector<T> (T(this->x - n), T(this->y - n), T(this->z - n), T(this->t - n));
  };


  template <class mT>
  GO4VECTOR_FUNCTION_PREFIX go4Vector<T> operator* (const go44Matrix<mT>& matrix) const
  {
      go4Vector<T> retValue;
      const mT* m = matrix.getPtr();
      retValue.x = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
      m += 4;
      retValue.y = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
      m += 4;
      retValue.z = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
      m += 4;
      retValue.w = *(m) * x + *(m + 1) * y + *(m + 2) * z + *(m + 3) * t;
      return retValue;
  };

  /*! @brief Cross-product.
   * Uses only the first three elements. */
  GO4VECTOR_FUNCTION_PREFIX void cross (const go4Vector<T>& other) 
  { 
      T x0,x1,x2; 
      x0 = y * other.z - z * other.y; 
      x1 = z * other.x - other.z * x; 
      x2 = x * other.y - other.x * y; 
      x = x0; 
      y = x1; 
      z = x2; 
  };

  GO4VECTOR_FUNCTION_PREFIX goDouble abs() const
    {
      return (goDouble)sqrt( (x * x) + 
			     (y * y) + 
			     (z * z) +
			     (t * t) );
    };

  /*!
   * Divides everything by t (for homog. coordinates).
   */
  GO4VECTOR_FUNCTION_PREFIX void div() 
    {
      T t_1 = (T)(1.0 / (goDouble)t);
      t = 1;
      x *= t_1;
      y *= t_1;
      z *= t_1;
    };

  friend std::ostream& operator<< (std::ostream &o, go4Vector<goDouble>& v);
  friend std::ostream& operator<< (std::ostream &o, go4Vector<goFloat>& v);

  T x, y, z;
  union
  { 
      T t;
      T w;
  };
};
#endif

typedef go4Vector<goFloat>  go4Vectorf;
typedef go4Vector<goDouble> go4Vectord;

/*! @} */

#undef GO4VECTOR_FUNCTION_PREFIX
#endif
