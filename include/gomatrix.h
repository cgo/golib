#ifndef __GOMATRIX_H
#define __GOMATRIX_H


#include <gotypes.h>

template<class T>
class goNVector;

/*!
 * Matrix class.
 */
template <class T>
class goMatrix {
 public:
  /*!
   * @param y Number of rows.
   * @param x Number of columns.
   */
  goMatrix(goSize_t y = 4, goSize_t x = 4);
  goMatrix(goMatrix<T>& other);
  virtual ~goMatrix ();

  inline T*		operator[] (goSize_t y) { return matrix[y]; }
  /*!
   * @return Number of columns
   */
  inline goSize_t	getSizeX() { return sizeX; }
  /*!
   * @return Number of rows
   */
  inline goSize_t	getSizeY() { return sizeY; }
  void				operator= (goMatrix<T> other);
  goMatrix<T>		operator* (goMatrix<T> other);
  goMatrix<T>		operator- (goMatrix<T> other);
  goMatrix<T>		operator+ (goMatrix<T> other);

  goMatrix<T>&		operator*= (goMatrix<T>& other);
  goMatrix<T>&		operator+= (goMatrix<T>& other);
  goMatrix<T>&		operator-= (goMatrix<T>& other);
  goMatrix<T>&		operator*= (T scalar);
  goMatrix<T>&		operator/= (T scalar);

  
  /*!
   * Matrix is filled with transpose. The dimensions are changed accordingly
   * and the memory is reallocated according to the new dimensions.  
   */
  void transpose();
  /// Loads unity
  void unity();
  void fill(T v);
  void print();

 protected:
  T **matrix;
  goSize_t sizeX;
  goSize_t sizeY;

};


#endif /* __GOMATRIX_H */
