/* Kill this line if you don't want --*- C++ -*-- */

#ifndef GO44MATRIX_H
#define GO44MATRIX_H

#include <gotypes.h>
// #include <go4vector.h>
#ifndef GOMATRIX_H
# include <gomatrix.h>
#endif
#include <golu.h>
#include <iostream>

/*! \addtogroup math
 * @{
 */
/*!
 * 4x4 Matrix.
 * @todo Either throw this class away or rework it a bit.
 */
template <class T>
class go44Matrix : public goMatrix<T> 
{
public:
  go44Matrix ()
      : goMatrix<T> (4,4)
  {
  };
  go44Matrix (T i11, T i12, T i13, T i14,
              T i21, T i22, T i23, T i24,
              T i31, T i32, T i33, T i34,
              T i41, T i42, T i43, T i44)
      : goMatrix<T> (4,4)
  {
      T* matrix = this->getData ();
      matrix[0] = i11;
      matrix[1] = i12;	
      matrix[2] = i13;
      matrix[3] = i14;
      matrix[4] = i21;
      matrix[5] = i22;	
      matrix[6] = i23;
      matrix[7] = i24;
      matrix[8] = i31;
      matrix[9] = i32;	
      matrix[10] = i33;
      matrix[11] = i34;
      matrix[12] = i41;
      matrix[13] = i42;	
      matrix[14] = i43;
      matrix[15] = i44;
  };
  go44Matrix (const go44Matrix<T>& other)
  {
      *this = other;
  };
  go44Matrix (const goMatrix<T>& other)
  {
      if (other.getRows() == 4 && other.getColumns() == 4)
      {
        *this = other;
      }
      else
      {
          assert (false);
      }
  };
  virtual ~go44Matrix ()
  {
  };

#if 0
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

  inline T& operator() (goIndex_t y, goIndex_t x)
  {
    return matrix[(y << 2) + x]; 
  }
  inline const T& operator() (goIndex_t y, goIndex_t x) const
  {
    return matrix[(y << 2) + x]; 
  }
  
  /*!
   * Fills the matrix with data found at source.
   */
  inline void 	fill (const T* source);

  inline void	fill (T value);

  /*!
   *
   */
  inline void   transpose ();

  inline bool   invert ();

  /*!
   * stream output for float matrices
   */
  friend std::ostream&        operator<< (std::ostream& o, go44Matrix<goFloat>&);
  friend std::ostream&        operator<< (std::ostream& o, go44Matrix<goDouble>&);
  friend std::ostream&        operator<< (std::ostream& o, const go44Matrix<goDouble>&);

  inline void operator= (const go44Matrix<T>& other);
  /*
   * Arithmetic operators
   */
  inline void  operator+= (const go44Matrix<T>& other);
  inline void  operator*= (goDouble r);
  inline void  operator*= (goInt32 r);
  inline void  operator*= (const go44Matrix<T>& other);
  // go4Vector<T>	operator* (go4Vector<T>& v);

  inline T* getPtr () { return matrix; }
  inline const T* getPtr () const { return (const T*)matrix; }
protected:
  T* matrix;
#endif
};

#if 0
template <class T>
class go44Matrix {
public:
  go44Matrix ()
  {
      matrix = new T[16];
  };
  go44Matrix (T i11, T i12, T i13, T i14,
              T i21, T i22, T i23, T i24,
              T i31, T i32, T i33, T i34,
              T i41, T i42, T i43, T i44)
  {
      matrix = new T[16];
      matrix[0] = i11;
      matrix[1] = i12;	
      matrix[2] = i13;
      matrix[3] = i14;
      matrix[4] = i21;
      matrix[5] = i22;	
      matrix[6] = i23;
      matrix[7] = i24;
      matrix[8] = i31;
      matrix[9] = i32;	
      matrix[10] = i33;
      matrix[11] = i34;
      matrix[12] = i41;
      matrix[13] = i42;	
      matrix[14] = i43;
      matrix[15] = i44;
  };
  go44Matrix (const go44Matrix<T>& other)
  {
      matrix = new T[16];
      *this = other;
  };
  ~go44Matrix ()
  {
      delete [] matrix;
      matrix = 0;
  };

  inline void setUnity ()
  {
     this->fill(T(0));
     this->matrix[0]  = T(1);
     this->matrix[5]  = T(1);
     this->matrix[10] = T(1);
     this->matrix[15] = T(1);
  };

  inline void setIdentity ()
  {
     this->setUnity ();
  };
  
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

  inline T& operator() (goIndex_t y, goIndex_t x)
  {
    return matrix[(y << 2) + x]; 
  }
  inline const T& operator() (goIndex_t y, goIndex_t x) const
  {
    return matrix[(y << 2) + x]; 
  }
  
  /*!
   * Fills the matrix with data found at source.
   */
  inline void 	fill (const T* source);

  inline void	fill (T value);

  /*!
   *
   */
  inline void   transpose ();

  inline bool   invert ();

  /*!
   * stream output for float matrices
   */
  friend std::ostream&        operator<< (std::ostream& o, go44Matrix<goFloat>&);
  friend std::ostream&        operator<< (std::ostream& o, go44Matrix<goDouble>&);
  friend std::ostream&        operator<< (std::ostream& o, const go44Matrix<goDouble>&);

  inline void operator= (const go44Matrix<T>& other);
  /*
   * Arithmetic operators
   */
  inline void  operator+= (const go44Matrix<T>& other);
  inline void  operator*= (goDouble r);
  inline void  operator*= (goInt32 r);
  inline void  operator*= (const go44Matrix<T>& other);
  // go4Vector<T>	operator* (go4Vector<T>& v);

  inline T* getPtr () { return matrix; }
  inline const T* getPtr () const { return (const T*)matrix; }
protected:
  T* matrix;
};
/*! @} */

template <class T>
inline
void
go44Matrix<T>::fill (const T* source) {
  T *tmp = matrix;
  const T *stmp = source;
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
bool
go44Matrix<T>::invert ()
{
    //= FIXME: This is a quick hack.
    goMatrix<T> M (4,4);
    for (goIndex_t i = 0; i < 4; ++i)
    {
        for (goIndex_t j = 0; j < 4; ++j)
        {
            M(i,j) = (*this)(i,j);
        }
    }
    goMatrix<T> B (4,4);
    B.setIdentity ();
    goMatrix<T> X (4,4);
    goMath::goLU<T> lu (M);
    if (!lu.solve (B,X))
    {
        return false;
    }
    
    for (goIndex_t i = 0; i < 4; ++i)
    {
        for (goIndex_t j = 0; j < 4; ++j)
        {
            (*this)(i,j) = X(i,j);
        }
    }
    return true;
}

template <class T>
inline
void
go44Matrix<T>::operator= (const go44Matrix<T>& other) 
{
  T *tmp = matrix;
  const T *stmp = other.getPtr();
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
go44Matrix<T>::operator+= (const go44Matrix<T>& other) 
{
  register goSize_t i = 0;
  register const T *otmp = other.getPtr();
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
      *__tmp = (T)(*__tmp * __r);				\
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
go44Matrix<T>::operator*= (const go44Matrix<T>& other)
{
    T a1,a2,a3,a4;
    const T *o = other.getPtr();
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
#endif

typedef go44Matrix<goFloat> go44Matrixf;
typedef go44Matrix<goDouble> go44Matrixd;

#endif /* __GO44MATRIX_H__ */
