// ostream& operator<< (ostream& o, goArray<goInt32>& a);
// ostream& operator<< (ostream& o, goArray<goUInt32>& a);

//#include <goarray.h>
//#include <gocomplex.h>
//#include <goconfigfile.h>
//#include <golist.h>
//#include <gohashtable.h>
#include <iostream>
#include <stdlib.h>   // realloc
#include <string.h>   // memset / memcpy

//#include <godepot.h>
//#include <gopaper.h>

template< class T >
goArray<T>::
goArray() {
  Array = 0;
  arraySize = 0;
}

template< class T >
goArray<T>::
goArray(goSize_t s) {
  Array = 0;
  arraySize = 0;
  this->resize (s);
}

template< class T >
goArray<T>::
goArray(const goArray<T>& other) {
  arraySize = 0;
  Array = 0;
  /*
  arraySize = other.getSize();
  Array = (T*)malloc(sizeof(T) * arraySize);
  goIndex_t i;
  for (i = 0; i < arraySize; i++) {
    Array[i] = other[i];
  }
  */
}

template< class T >
goArray<T>::
~goArray() {
  if (Array) {
    free (Array);
  }
}

template< class T >
goIndex_t
goArray<T>::
resize(goIndex_t size) {
  Array = (T*)realloc((void*)Array,sizeof(T) * size);
  arraySize = size;
  return (goIndex_t) (sizeof(Array) / (float)sizeof(T));
}

template < class T >
void
goArray<T>::
quickSort (goIndex_t i1, goIndex_t i2) {
  goIndex_t l = i1, r = i2;
  T temp;
  // cout << "called with " << l << "," << r << endl;
  if (r <= l)
    {
      return;
    }
  T ref = (*this)[ (r + l) >> 1 ];
  while (l <= r) {
    while ( (l < i2) && ((*this)[l] < ref) ) l++;
    while ( (r > i1) && ((*this)[r] > ref) ) r--;
//      cout << "ref = " << ref << endl;
//      cout << "l = " << l << ", r = " << r << endl;
    if (l <= r) {
//        cout << "swapping " << (*this)[l] << "," << (*this)[r] << endl;
      temp = (*this)[l];
      (*this)[l] = (*this)[r];
      (*this)[r] = temp;
      l++; r--;
    }
  }
  if (l < i2)
    quickSort (l,i2);
  if (r > i1)
    quickSort (i1,r);
}

// Slow sort routine
template < class T >
void
goArray<T>::
sort () {
  quickSort (0, getSize() - 1);
}

template < class T >
void
goArray<T>::
quickSort (goIndex_t i1, goIndex_t i2, goArray<int>& indexArray) {
  goIndex_t l = i1, r = i2;
  T temp;
  int tempIdx;
  // cout << "called with " << l << "," << r << endl;
  if (r <= l)
    {
      return;
    }
  T ref = (*this)[ (r + l) >> 1 ];
  while (l <= r) {
    while ( (l < i2) && ((*this)[l] < ref) ) l++;
    while ( (r > i1) && ((*this)[r] > ref) ) r--;
//      cout << "ref = " << ref << endl;
//      cout << "l = " << l << ", r = " << r << endl;
    if (l <= r) {
//        cout << "swapping " << (*this)[l] << "," << (*this)[r] << endl;
      temp = (*this)[l];
      (*this)[l] = (*this)[r];
      (*this)[r] = temp;
      tempIdx = indexArray[l];
      indexArray[l] = indexArray[r];
      indexArray[r] = tempIdx;
      l++; r--;
    }
  }
  if (l < i2)
    quickSort (l,i2,indexArray);
  if (r > i1)
    quickSort (i1,r,indexArray);
}


template < class T >
void
goArray<T>::
quickSort (goIndex_t i1, goIndex_t i2, goArray<goSize_t>& indexArray) {
  goIndex_t l = i1, r = i2;
  T temp;
  goSize_t tempIdx;
  // cout << "called with " << l << "," << r << endl;
  if (r <= l)
    {
      return;
    }
  T ref = (*this)[ (r + l) >> 1 ];
  while (l <= r) {
    while ( (l < i2) && ((*this)[l] < ref) ) l++;
    while ( (r > i1) && ((*this)[r] > ref) ) r--;
//      cout << "ref = " << ref << endl;
//      cout << "l = " << l << ", r = " << r << endl;
    if (l <= r) {
//        cout << "swapping " << (*this)[l] << "," << (*this)[r] << endl;
      temp = (*this)[l];
      (*this)[l] = (*this)[r];
      (*this)[r] = temp;
      tempIdx = indexArray[l];
      indexArray[l] = indexArray[r];
      indexArray[r] = tempIdx;
      l++; r--;
    }
  }
  if (l < i2)
    quickSort (l,i2,indexArray);
  if (r > i1)
    quickSort (i1,r,indexArray);
}

template < class T >
void
goArray<T>::
quickSort (goIndex_t i1, goIndex_t i2, goArray<void*>& indexArrays) {
  goIndex_t l = i1, r = i2;
  T temp;
  goSize_t tempIdx;
  goSize_t i;
  // cout << "called with " << l << "," << r << endl;
  if (r <= l)
    {
      return;
    }
  T ref = (*this)[ (r + l) >> 1 ];
  while (l <= r) {
    while ( (l < i2) && ((*this)[l] < ref) ) l++;
    while ( (r > i1) && ((*this)[r] > ref) ) r--;
//      cout << "ref = " << ref << endl;
//      cout << "l = " << l << ", r = " << r << endl;
    if (l <= r) {
//        cout << "swapping " << (*this)[l] << "," << (*this)[r] << endl;
      temp = (*this)[l];
      (*this)[l] = (*this)[r];
      (*this)[r] = temp;
      for (i = 0; i < (goSize_t)indexArrays.getSize(); i++)
	{
	  tempIdx = (*((goArray<goSize_t>*)indexArrays[i]))[l];
	  (*((goArray<goSize_t>*)indexArrays[i]))[l] = (*((goArray<goSize_t>*)indexArrays[i]))[r];
	  (*((goArray<goSize_t>*)indexArrays[i]))[r] = tempIdx;
	}
      l++; r--;
    }
  }
  if (l < i2)
    quickSort (l,i2,indexArrays);
  if (r > i1)
    quickSort (i1,r,indexArrays);
}

// Slow sort routine
template < class T >
void
goArray<T>::
sort (goArray<int>& indexArray) {
  quickSort (0, getSize() - 1, indexArray);
}

// Slow sort routine
template < class T >
void
goArray<T>::
sort (goArray<goSize_t>& indexArray) {
  quickSort (0, getSize() - 1, indexArray);
}

// Slow sort routine
template < class T >
void
goArray<T>::
sort (goArray<void*>& indexArrays) {
  quickSort (0, getSize() - 1, indexArrays);
}


template <class T>
void
goArray<T>::byteFill (int f)
{
    memset ((void*)Array, f, sizeof(T) * getSize());
}

template <class T>
void
goArray<T>::fill (T value)
{
	goIndex_t i;
	for (i = 0; i < getSize(); i++)
	{
		Array[i] = value;
	}
}

template <class T>
void
goArray<T>::remove (goIndex_t index)
{
    goIndex_t i;
    for (i = index + 1; i < getSize(); i++)
	{
	    Array[i-1] = Array[i];
	}
    this->resize (this->getSize() - 1);
}


template< class T >
goArray<T>&
goArray<T>::
operator= (goArray<T>& other) {
  goIndex_t i = 0;
  this->resize (other.getSize());
  // Faster version
  memcpy ((void*)this->getPtr(), (void*)other.getPtr(), sizeof(T) * this->getSize());
  return (*this);
  
  for (i = 0; i < this->getSize(); i++) {
    (*this)[i] = other[i];
  }
  return (*this);
}

template< class T >
goArray<T>&
goArray<T>::
operator= (const goArray<T>& other) {
  goIndex_t i = 0;
  this->resize (other.getSize());
  for (i = 0; i < this->getSize(); i++) {
    (*this)[i] = other[i];
  }
  return (*this);
}

template< class T >
goArray<T>&
goArray<T>::
operator+= (T item) {
  this->resize (this->getSize()+1);
  (*this)[this->getSize()-1] = item;
  return (*this);
}

template< class T >
bool
goArray<T>::
operator== (goArray<T>& other) {
  if (other.getSize() != this->getSize()) return false;
  goIndex_t i;
  for (i = 0; i < getSize(); i++) {
    if ( (*this)[i] != other[i] ) return false;
  }
  return true;
}

template< class T >
bool
goArray<T>::
operator!= (goArray<T>& other) {
  return !( (*this) == other );
}
