#include <gomatrix.h>
#include <gomath.h>		// MAX()
#include <iostream>

template <class T>
goMatrix<T>::goMatrix (goSize_t y, goSize_t x) {
  sizeX = x;
  sizeY = y;

  goSize_t i;
  matrix = new T*[y];
  for (i = 0; i < y; i++) {
    matrix[i] = new T[x];
  }
}

template<class T>
goMatrix<T>::goMatrix(goMatrix<T>& other)
{
  sizeX = other.getSizeX();
  sizeY = other.getSizeY();

  goSize_t i;
  matrix = new T*[sizeY];
  for (i = 0; i < sizeY; i++) {
    matrix[i] = new T[sizeX];
  }
  
  goSize_t x,y;
  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      (*this)[y][x] = other[y][x];
    }
  }
}

template <class T>
goMatrix<T>::~goMatrix () {
  goSize_t i;
  for (i = 0; i < sizeY; i++) {
    delete[] matrix[i];
  }
  delete[] matrix;
}

template <class T>
void
goMatrix<T>::operator= (goMatrix<T> other) {
  goSize_t x,y;
  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      (*this)[y][x] = other[y][x];
    }
  }
}

template <class T>
goMatrix<T>
goMatrix<T>::operator* (goMatrix<T> other) {
  goMatrix<T> retval(sizeY, other.getSizeX());
  goSize_t x, y, x2;
  goSize_t sizex = other.getSizeX();
  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizex; x++) {
      retval[y][x] = 0;
      for (x2 = 0; x2 < sizeX; x2++) {
		retval[y][x] += (*this)[y][x2] * other[x2][x]; 
      }
    }
  } 
  return retval;
}

template <class T>
goMatrix<T>
goMatrix<T>::operator- (goMatrix<T> other) {
  goMatrix<T> retval(sizeY, sizeX);
  goSize_t x, y;
  for (y = 0; y < sizeY; y++) 
  {
    for (x = 0; x < sizeX; x++) 
	{
		retval[y][x] = (*this)[y][x] - other[y][x]; 
    }
  } 
  return retval;
}

template <class T>
goMatrix<T>
goMatrix<T>::operator+ (goMatrix<T> other) {
  goMatrix<T> retval(sizeY, sizeX);
  goSize_t x, y;
  for (y = 0; y < sizeY; y++) 
  {
    for (x = 0; x < sizeX; x++) 
	{
		retval[y][x] = (*this)[y][x] + other[y][x]; 
    }
  } 
  return retval;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator*= (goMatrix<T>& other) 
{
  goMatrix<T> m (sizeY, sizeX);
  register goSize_t x, y, x2;
  register goSize_t sizex = sizeX;
  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizex; x++) {
      m[y][x] = 0;
      for (x2 = 0; x2 < sizex; x2++) {
		m[y][x] += (*this)[y][x2] * other[x2][x]; 
      }
    }
  } 
  *this = m;
  return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator+= (goMatrix<T>& other) {
  goSize_t x,y;

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      (*this)[y][x] += other[y][x];
    }
  }
  return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator-= (goMatrix<T>& other) {
  goSize_t x,y;

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      (*this)[y][x] -= other[y][x];
    }
  }
  return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator*= (T scalar) {
  goSize_t x,y;

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      (*this)[y][x] *= scalar;
    }
  }
  return *this;
}

template <class T>
goMatrix<T>&
goMatrix<T>::operator/= (T scalar) {
  goSize_t x,y;

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      (*this)[y][x] /= scalar;
    }
  }
  return *this;
}

template <class T>
void
goMatrix<T>::transpose() {
  goMatrix<T> temp(sizeY, sizeX);
  temp = *this;

  
  goSize_t i;
  for (i = 0; i < sizeY; i++) {
    delete[] matrix[i];
  }
  delete[] matrix;

  goSize_t x, y, newX = sizeY;
  sizeY = sizeX;
  sizeX = newX;

  matrix = new T*[sizeY];
  for (i = 0; i < sizeY; i++) {
    matrix[i] = new T[sizeX];
  }

  for (y = 0; y < sizeY; y++) {
    for (x = 0; x < sizeX; x++) {
      (*this)[y][x] = temp[x][y];
    }
  }
  
}

template<class T>
void
goMatrix<T>::unity()
{
	fill(0);
	goSize_t n = MAX(sizeX,sizeY);
	goSize_t i;
	for (i = 0; i < n; i++)
	{
		matrix[i][i] = 1;
	}
}

template<class T>
void
goMatrix<T>::fill(T v)
{
	goSize_t x,y;
	for (y = 0; y < sizeY; y++)
	{
		for (x = 0; x < sizeX; x++)
		{
			matrix[y][x] = v;
		}
	}
}

template<class T>
void
goMatrix<T>::print()
{
	unsigned int i,j;
	for (i = 0; i < sizeY; i++)
	{
		for (j = 0; j < sizeX; j++)
		{
            std::cout << (*this)[i][j] << " ";
		}
        std::cout << "\n";
	}
    std::cout << std::endl;
}

/* Instantiation */
template class goMatrix<goDouble>;
template class goMatrix<goFloat>;
template class goMatrix<goInt32>;

