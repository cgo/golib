#include <gonvector.h>
#include <goarray.h>
#include <gocomplex.h>

// ostream& operator<< (ostream& o, class goComplex<goDouble> >& c);

template <class T>
goNVector<T>::goNVector (goUInt32 n) : goArray<T> () {
  delete_vector = true;
  absValid = false;
  resize (n);
}

template <class T>
goNVector<T>::goNVector () : goArray<T> () {
  delete_vector = true;
  absValid = false;
}

template <class T>
goNVector<T>::~goNVector () {
  if (delete_vector) {
    // delete Array;
	// Dumbass...
  }
}

template <class T>
inline
void
goNVector<T>::setVectorPtr (T* ptr) {
  if (delete_vector && Array) {
    delete Array;
  }
  Array = ptr;
  delete_vector = false;
  absValid = false;
}

// template<class T>
// T&
// goNVector<T>::
// operator[] (int idx) {
//   return vector[idx];
// }

template<class T>
inline
goNVector<T>& 
goNVector<T>::
operator+= (goNVector<T>& other) {
  if (this->getSize() == other.getSize()) {
    int i = 0;
    for (i = 0; i < arraySize; i++) {
      Array[i] += other[i];
    }
  }
  return *this;
}

template<class T>
inline
goNVector<T>& 
goNVector<T>::
operator-= (goNVector<T>& other) {
  if (this->getSize() == other.getSize()) {
    int i = 0;
    for (i = 0; i < arraySize; i++) {
      Array[i] -= other[i];
    }
  }
  return *this;
}

inline
double
goNVector<double>::
abs () {
  goIndex_t i = 0;
  
  if (!absValid) {
    absValue = 0;
    for (i = 0; i < getSize(); i++) {
      absValue += Array[i] * Array[i];
    }
    absValue = (double)sqrt ((double)absValue);
    absValid = true;
  }
  return absValue;
}

template <class T>
ostream& operator<< (ostream& ostr, goNVector<T>& vec) {
  goIndex_t i = 0;
  for (i = 0; i < (vec.getSize() - 1); i++) {
    ostr << vec[i] << ",";
  }
  ostr << vec[i] << "\n";
  return ostr;
}

ostream& operator<< (ostream& ostr, goNVector<int>& vec) {
  goIndex_t i = 0;
  for (i = 0; i < (vec.getSize() - 1); i++) {
    ostr << vec[i] << ",";
  }
  ostr << vec[i] << "\n";
  return ostr;
}

ostream& operator<< (ostream& ostr, goNVector<goInt16>& vec) {
  goIndex_t i = 0;
  for (i = 0; i < (vec.getSize() - 1); i++) {
    ostr << vec[i] << ",";
  }
  ostr << vec[i] << "\n";
  return ostr;
}

ostream& operator<< (ostream& ostr, goNVector<goDouble>& vec) {
  goIndex_t i = 0;
  for (i = 0; i < (vec.getSize() - 1); i++) {
    ostr << vec[i] << ",";
  }
  ostr << vec[i] << "\n";
  return ostr;
}

ostream& operator<< (ostream& ostr, goNVector<goComplex<goDouble> >& vec) {
  goIndex_t i = 0;
  for (i = 0; i < (vec.getSize() - 1); i++) {
    ostr << vec[i] << ",";
  }
  ostr << vec[i] << "\n";
  return ostr;
}


/* Instantiation */
template class goNVector<goUInt32>;
template class goNVector<goInt32>;
template class goNVector<goInt16>;
template class goNVector<goFloat>;
template class goNVector<goDouble>;
template class goNVector<unsigned char>;

template class goNVector<goComplex<goDouble> >;







