/* Kill this line if you don't want --*- C++ -*-- */

#ifndef GO44MATRIX_H
#define GO44MATRIX_H

#include <gotypes.h>
// #include <go4vector.h>

#include <iostream>

/*!
 * 4x4 Matrix.
 */
template <class T>
class go44Matrix {
public:
  go44Matrix ();
  go44Matrix (T, T, T, T,
	      T, T, T, T,
	      T, T, T, T,
	      T, T, T, T);
  go44Matrix (go44Matrix<T>& other);
  go44Matrix (const go44Matrix<T>& other);
  ~go44Matrix ();

  /*!
   * @return Reference to object at [y][x]
   */
  inline T& 	elem (goIndex_t y, goIndex_t x) 
  { 
    return matrix[(y << 2) + x]; 
  }

  inline const T& 	elem (goIndex_t y, goIndex_t x)  const
  { 
    return matrix[(y << 2) + x]; 
  }

  /*!
   * Fills the matrix with data found at source.
   */
  inline void 	fill (T* source);

  inline void	fill (T value);

  /*!
   *
   */
  inline void   transpose ();

  /*!
   * stream output for float matrices
   */
  friend std::ostream&        operator<< (std::ostream& o, go44Matrix<goFloat>&);
  friend std::ostream&        operator<< (std::ostream& o, go44Matrix<goDouble>&);
  friend std::ostream&        operator<< (std::ostream& o, const go44Matrix<goDouble>&);

  inline void operator= (go44Matrix<T>& other);
  inline void operator= (const go44Matrix<T>& other);
  /*
   * Arithmetic operators
   */
  inline void  operator+= ( go44Matrix<T>& other);
  inline void  operator*= (goDouble r);
  inline void  operator*= (goInt32 r);
  inline void  operator*= ( go44Matrix<T>& other);
  // go4Vector<T>	operator* (go4Vector<T>& v);

  inline T* getPtr () { return matrix; }
  inline const T* getConstPtr () const { return (const T*)matrix; }
protected:
  T* matrix;
};


template <class T>
inline
void
go44Matrix<T>::fill (T* source) {
  T *tmp = matrix;
  T *stmp = source;
  goSize_t i;
  for (i = 0; i < 16; i++)
    {
      *(tmp++) = *(stmp++);
    }
}

template <class T>
inline
void
go44Matrix<T>::fill (T value) {
  T *tmp = matrix;
  goSize_t i;
  for (i = 0; i < 16; i++)
    {
      *(tmp) = value;
      tmp++;
    }
}


template <class T>
inline
void
go44Matrix<T>::transpose () 
{
  T r1[4];
  T r2[4];
  T r3[4];
  T r4[4];
  goSize_t i;
  T *m = matrix;
  for (i = 0; i < 4; i++)
    {
      r1[i] = *m;
      r2[i] = *(m + 4);
      r3[i] = *(m + 8);
      r4[i] = *(m + 12);
      m++;
    }
  m = matrix;
  for (i = 0; i < 4; i++)
    {
      *(m) = r1[i]; m++;
      *(m)  = r2[i]; m++;
      *(m)  = r3[i]; m++;
      *(m) = r4[i]; m++;
    }
}

template <class T>
inline
void
go44Matrix<T>::operator= (go44Matrix<T>& other) 
{
  T *tmp = matrix;
  T *stmp = other.getPtr();
  goSize_t i;
  for (i = 0; i < 16; i++)
    {
      *(tmp) = *(stmp);
      tmp++; stmp++;
    }
}

template <class T>
inline
void
go44Matrix<T>::operator= (const go44Matrix<T>& other) 
{
  T *tmp = matrix;
  const T *stmp = other.getConstPtr();
  goSize_t i;
  for (i = 0; i < 16; i++)
    {
      *(tmp) = *(stmp);
      tmp++; stmp++;
    }
}

template <class T>
inline
void
go44Matrix<T>::operator+= (go44Matrix<T>& other) 
{
  register goSize_t i = 0;
  register T *otmp = other.getPtr();
  register T *tmp = matrix;
  for (i = 0; i < 16; i++) {
    *(tmp) += *(otmp);
    tmp++; otmp++;
  }
}

#define GO_44MATRIX_MULT(__type,__r) {		\
  T* __tmp = matrix;				\
  goSize_t __i;					\
  for (__i = 0; __i < 16; __i++)		\
    {						\
      *__tmp *= __r;				\
      __tmp++;					\
    }						\
}

template <class T>
inline
void
go44Matrix<T>::operator*= (goDouble r)
{
  GO_44MATRIX_MULT(goDouble,r);
} 

template <class T>
inline
void
go44Matrix<T>::operator*= (goInt32 r)
{
  GO_44MATRIX_MULT(goInt32,r);
} 

template <class T>
inline
void
go44Matrix<T>::operator*= (go44Matrix<T>& other)
{
  T a1,a2,a3,a4;
  T *o = other.getPtr();
  T *m = matrix;
  goSize_t i,j;
  for (i = 0; i < 4; i++)
    {
      a1 = *m;
      a2 = *(m + 1);
      a3 = *(m + 2);
      a4 = *(m + 3);
      for (j = 0; j < 4; j++)
	{
	  *m = a1 * (*o) + 
	    a2 * (*(o + 4)) + 
	    a3 * (*(o + 8)) + 
	    a4 * (*(o + 12));
	  m++; o++;
	}
      o -= 4;
    }
} 


#endif /* __GO44MATRIX_H__ */








