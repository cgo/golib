/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOMATRIX_I
#define GOMATRIX_I

#ifndef GOMATH_H
# include <gomath.h>
#endif

template <class T>
bool
goMatrix<T>::initializeRows ()
{
    assert (this->matrix);
    if (this->rows)
    {
        delete[] this->rows;
        this->rows = NULL;
    }
    if (this->rowVectors)
    {
        delete[] this->rowVectors;
        this->rowVectors = NULL;
    }
    this->rows       = new goSubSignal3D<T> [this->getRows()];
    this->rowVectors = new goRowVector<T>   [this->getRows()];
    if (!this->rows || !this->rowVectors)
    {
        assert (false);
        return false;
    }
    goSize_t i;
    goSize3D rowSize (this->getColumns(), 1, 1);
    for (i = 0; i < this->getRows(); ++i)
    {
        this->rows[i].setSize        (rowSize);
        this->rows[i].setParent      (this->matrix);
        this->rows[i].setPosition    (0, i, 0);
        this->rowVectors[i].setData  (&this->rows[i]);
    }
    return true;
}

template <class T>
goRowVector<T>&
goMatrix<T>::operator [] (goSize_t row)
{
    assert (this->rowVectors);
    return this->rowVectors [row];
}

template <class T>
const goRowVector<T>&
goMatrix<T>::operator [] (goSize_t row) const
{
    assert (this->rowVectors);
    return this->rowVectors [row];
}

//===================================================================

template <class T> 
goMatrix<T>::goMatrix (goSignal3DBase<T>* data)
    :
    externalData (true),
    matrix       (data),
    rows         (NULL),
    rowVectors   (NULL)
{
    initializeRows ();
}

template <class T>
goMatrix<T>::goMatrix (goSize_t rows, goSize_t cols)
    :
    externalData (false),
    matrix       (NULL),
    rows         (NULL),
    rowVectors   (NULL)
{
    matrix = (goSignal3DBase<T>*) new goSignal3D<T> (cols, rows, 1);
    initializeRows ();
}

template<class T>
goMatrix<T>::goMatrix(const goMatrix<T>& other)
    :
    externalData (false),
    matrix       (NULL),
    rows         (NULL),
    rowVectors   (NULL)
{
    assert (other.matrix != NULL);
    matrix = (goSignal3DBase<T>*) new goSignal3D<T> (other.getColumns(), other.getRows(), 1);
    if (other.getSizeX() == getSizeX() &&
        other.getSizeY() == getSizeY())
    {
        // std::cout << "Copy constructor...\n";
        GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*matrix), (*other.matrix), T, T);
    }
    initializeRows ();
}

template <class T>
goMatrix<T>::~goMatrix () 
{
    if (!externalData && matrix)
    {
        delete matrix;
        matrix = NULL;
    }
    if (this->rows)
    {
        delete [] this->rows;
        this->rows = NULL;
    }
    if (this->rowVectors)
    {
        delete [] this->rowVectors;
        this->rowVectors = NULL;
    }
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


// TNT compatibility methods BEGIN

template <class T>
int
goMatrix<T>::dim1 () const
{
    return (int)getRows ();
}

template <class T>
int
goMatrix<T>::dim2 () const
{
    return (int)getColumns ();
}

template <class T>
const goMatrix<T>&
goMatrix<T>::copy () const
{
    return *this;
}

// TNT compatibility methods END  

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
goMatrix<T>&
goMatrix<T>::operator= (const goMatrix<T>& other) 
{
    assert (matrix);
    if (other.getSizeX() != getSizeX() ||
        other.getSizeY() != getSizeY())
    {
        this->resize (other.getRows(), other.getColumns());
    }
    std::cout << "goMatrix<> operator=()...\n";
    GO_SIGNAL3D_EACHELEMENT_2 (*__ptr = *__ptr_target, (*matrix), (*other.matrix), T, T);
    initializeRows ();
    return *this;
}

template <class T>
goMatrix<T>
goMatrix<T>::operator* (const goMatrix<T>& other) 
{
  if (getColumns() != other.getRows())
  {
      goError::print ("goMatrix::operator*","Matrix dimensions do not match.");
      return goMatrix<T> (1,1);
  }
  goMatrix<T> retval (getRows(), other.getColumns());
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
    initializeRows ();
}

template<class T>
void
goMatrix<T>::identity ()
{
	fill(0);
	goSize_t n = MAX(matrix->getSizeX(),matrix->getSizeY());
	goSize_t i;
	for (i = 0; i < n; ++i)
	{
		(*this)[i][i] = (T)1;
	}
}

template<class T>
void
goMatrix<T>::unity ()
{
    this->identity ();
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

template <class T>
bool goMatrix<T>::resize (goSize_t rows, goSize_t columns)
{
    assert (this->matrix);
    if (this->externalData)
    {
        // Will not resize external data -- data organization is unknown!
        return false;
    }
    this->matrix->destroy ();
    ((goSignal3D<T>*)this->matrix)->make (columns, rows, 1);
    return initializeRows ();
}

template <class T>
bool goMatrix<T>::setData (goSignal3DBase<T>* data)
{
    if (!this->externalData && this->matrix)
    {
        delete this->matrix;
        this->matrix = NULL;
    }
    this->matrix = data;
    this->externalData = true;
    return initializeRows ();
}

#endif
