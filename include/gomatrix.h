#ifndef __GOMATRIX_H
#define __GOMATRIX_H


#include <gotypes.h>
#include <gosignal3dbase.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>

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
  goMatrix (const goMatrix<T>& other);
  virtual ~goMatrix ();

 protected:
  goMatrix(goSignal3DBase<T>* data);

 public:
  /*!
   * This is inefficient, use it as little as possible.
   */
  inline goMatrix<T>* operator[] (goSize_t y);
  inline goSize_t   getColumns() const;
  inline goSize_t   getRows()    const;

  inline T&         elem (goSize_t i, goSize_t j);
  /*!
   * @return Number of columns
   */
  inline goSize_t	getSizeX() const; 
  /*!
   * @return Number of rows
   */
  inline goSize_t	getSizeY() const;
  
  goMatrix<T>&      operator= (const goMatrix<T>& other);
  goMatrix<T>		operator* (const goMatrix<T>& other);
  goMatrix<T>		operator- (const goMatrix<T>& other);
  goMatrix<T>		operator+ (const goMatrix<T>& other);

  goMatrix<T>&		operator*= (const goMatrix<T>& other);
  goMatrix<T>&		operator+= (const goMatrix<T>& other);
  goMatrix<T>&		operator-= (const goMatrix<T>& other);
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
  bool               externalData;
  goSignal3DBase<T>* matrix;

};

template <class T> 
goMatrix<T>::goMatrix (goSignal3DBase<T>* data)
    :
    externalData (true),
    matrix       (data)
{
}

/*!
 * @return A newly allocated goMatrix object which contains the \c row'th row of this matrix.
 * @note Do not forget to delete the returned object when it is no longer needed. There is
 * no mechanism as of yet that would do this for you.
 */
template <class T>
goMatrix<T>*
goMatrix<T>::operator [] (goSize_t row)
{
    goSubSignal3D<T>* vector = new goSubSignal3D<T> (matrix, matrix->getSizeX(), 1, 1);
    vector.setPosition (0, row, 0);
    goMatrix<T>* retMatrix = new goMatrix<T> (vector);
    retMatrix->externalData = false;

    return retMatrix;
}

template <class T> 
goSize_t
goMatrix<T>::getColumns() const
{
    return getSizeX();
}

template <class T>  
goSize_t
goMatrix<T>::getRows() const
{
    return getSizeY();
}

template <class T>
T&
goMatrix<T>::elem (goSize_t i, goSize_t j)
{
    return *(T*)matrix->getPtr (j,i,0);
}

template <class T>
goSize_t
goMatrix<T>::getSizeX() const
{
    if (matrix)
    {
        return matrix->getSizeX();
    }
    return 0;
}

template <class T>
goSize_t
goMatrix<T>::getSizeY() const
{
    if (matrix)
    {
        return matrix->getSizeY();
    }
    return 0;
}

//===================================================================

template <class T>
goMatrix<T>::goMatrix (goSize_t rows, goSize_t cols)
    :
    externalData (false),
    matrix       (NULL)
{
    matrix = (goSignal3DBase<T>*) new goSignal3D<T> (cols, rows, 1);
}

template<class T>
goMatrix<T>::goMatrix(const goMatrix<T>& other)
    :
    externalData (false),
    matrix       (NULL)
{
    assert (other.matrix != NULL);
    matrix = (goSignal3DBase<T>*) new goSignal3D<T> (other.getColumns(), other.getRows(), 1);
    if (other.getSizeX() == getSizeX() &&
        other.getSizeY() == getSizeY())
    {
        std::cout << "Copy constructor...\n";
        GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*matrix), (*other.matrix), T, T);
    }
}

template <class T>
goMatrix<T>::~goMatrix () 
{
    if (!externalData && matrix)
    {
        delete matrix;
        matrix = NULL;
    }
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator= (const goMatrix<T>& other) 
{
    assert (matrix);
    if (other.getSizeX() == getSizeX() &&
        other.getSizeY() == getSizeY())
    {
        std::cout << "matrix operator=()...\n";
        GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*matrix), (*other.matrix), T, T);
    }
    return *this;
}

template <class T>
goMatrix<T>
goMatrix<T>::operator* (const goMatrix<T>& other) 
{
  goMatrix<T> retval(getRows(), other.getColumns());
  goSize_t x, y, x2;
  goSize_t columns = retval.getColumns();
  goSize_t thisColumns = this->getColumns();
  for (y = 0; y < retval.getRows(); ++y) 
  {
    for (x = 0; x < columns; ++x) 
    {
      T* p  = this->matrix->getPtr (0, y, 0);
      T* po = other.matrix->getPtr (x, 0, 0);
      goPtrdiff_t* pDiff = this->matrix->getXDiff();
      goPtrdiff_t* poDiff = other.matrix->getYDiff();
      T value = (T)0; 
      for (x2 = 0; x2 < thisColumns; ++x2)
      {
          value += *p * *po;
          p += *pDiff;
          po += *poDiff;
          ++pDiff;
          ++poDiff;
      }
      retval.elem(y,x) = value;      
    }
  } 
  return retval;
}

// FIXME
template <class T>
goMatrix<T>
goMatrix<T>::operator- (const goMatrix<T>& other) {
  goMatrix<T> retval(getRows(), getColumns());
  goSize_t x, y;
  goSize_t columns = this->getColumns();
  for (y = 0; y < retval.getRows(); y++) 
  {
      T* p  = this->matrix->getPtr  (0, y, 0);
      T* po = other.matrix->getPtr  (0, y, 0);
      T* pr = retval.matrix->getPtr (0, y, 0);
      goPtrdiff_t* pDiff = this->matrix->getXDiff();
      goPtrdiff_t* poDiff = other.matrix->getXDiff();
      goPtrdiff_t* prDiff = retval.matrix->getXDiff();
      for (x = 0; x < columns; ++x)
      {
          *pr = *p - *po;
          p += *pDiff; po += *poDiff; pr += *prDiff;
          ++prDiff;
          ++poDiff;
          ++pDiff;
      }
  } 
  return retval;
}

template <class T>
goMatrix<T>
goMatrix<T>::operator+ (const goMatrix<T>& other) {
  goMatrix<T> retval(getRows(), getColumns());
  goSize_t x, y;
  goSize_t columns = this->getColumns();
  for (y = 0; y < retval.getRows(); y++) 
  {
      T* p  = this->matrix->getPtr  (0, y, 0);
      T* po = other.matrix->getPtr  (0, y, 0);
      T* pr = retval.matrix->getPtr (0, y, 0);
      goPtrdiff_t* pDiff = this->matrix->getXDiff();
      goPtrdiff_t* poDiff = other.matrix->getXDiff();
      goPtrdiff_t* prDiff = retval.matrix->getXDiff();
      for (x = 0; x < columns; ++x)
      {
          *pr = *p + *po;
          p += *pDiff; po += *poDiff; pr += *prDiff;
          ++prDiff;
          ++poDiff;
          ++pDiff;
      }
  } 
  return retval;
}

// FIXME
// This is a quick hack, very slow
template <class T>
goMatrix<T>&
goMatrix<T>::operator*= (const goMatrix<T>& other) 
{
   goMatrix<T> m = *this * other;
   *this = m;
   return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator+= (const goMatrix<T>& other) 
{
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr += *__ptr_target, (*this->matrix), (*other.matrix), T, T);
    return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator-= (const goMatrix<T>& other) {
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr += *__ptr_target, (*this->matrix), (*other.matrix), T, T);
    return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator*= (T scalar) 
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr *= scalar, (*matrix), T);
    return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator/= (T scalar) {
    GO_SIGNAL3D_EACHELEMENT (*__ptr /= scalar, (*matrix), T);
    return *this;
}

template <class T>
void
goMatrix<T>::transpose() 
{
    matrix->swapXY();
}

template<class T>
void
goMatrix<T>::unity()
{
	fill(0);
	goSize_t n = MAX(matrix->getSizeX(),matrix->getSizeY());
	goSize_t i;
	for (i = 0; i < n; ++i)
	{
		*(T*)matrix->getPtr(i,i) = (T)1;
	}
}

template<class T>
void
goMatrix<T>::fill(T v)
{
    GO_SIGNAL3D_EACHELEMENT (*__ptr = v, (*matrix), T);
}

template<class T>
void
goMatrix<T>::print()
{
	unsigned int i,j;
    goSize_t sizeX = matrix->getSizeX();
    goSize_t sizeY = matrix->getSizeY();
	for (i = 0; i < sizeY; i++)
	{
		for (j = 0; j < sizeX; j++)
		{
            std::cout << *(T*)matrix->getPtr(j,i,0) << " ";
		}
        std::cout << "\n";
	}
    std::cout << std::endl;
}

#endif /* __GOMATRIX_H */
