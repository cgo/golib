#include <gocomplex.h>
#include <math.h>
#include <gotypes.h>


template <class T>
goComplex<T>::goComplex () {
  real = 0;
  ima  = 0;
  argValue = 0;
  absValue = 0;
  info.argValid = false;
  info.absValid = false;
}

template <class T>
goComplex<T>::goComplex (T r, T i) {
  real = r;
  ima  = i;
  argValue = 0;
  absValue = 0;
  info.argValid = false;
  info.absValid = false;
}

template <class T>
goComplex<T>::goComplex (goComplex<T>& other) {
  real = other.re ();
  ima  = other.im ();
  argValue = 0;
  absValue = 0;
  info.argValid = false;
  info.absValid = false;
}

template <class T>
goComplex<T>::~goComplex () {
}

template <class T>
goComplex<T>
goComplex<T>::operator* (goComplex<T>& other) {
  goComplex<T> retval;

  retval.re() = (real * other.re()) - (ima * other.im());
  retval.im() = (real * other.im()) + (ima * other.re());

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator*= (goComplex<T>& other) {
  T tmpreal;
  T tmpima;

  tmpreal = (real * other.re()) - (ima * other.im());
  tmpima  = (real * other.im()) + (ima * other.re());

  real = tmpreal;
  ima  = tmpima;
  info.argValid = false;
  info.absValid = false;
  return *this;
}

template <class T>
goComplex<T>
goComplex<T>::operator+ (goComplex<T>& other) {
  goComplex<T> retval;

  retval.re() = real + other.re();
  retval.im() = ima  + other.im();

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator+= (goComplex<T>& other) {
  real += other.re();
  ima  += other.im();
  info.argValid = false;
  info.absValid = false;
  return *this;
}

template <class T>
goComplex<T>
goComplex<T>::operator- (goComplex<T>& other) {
  goComplex<T> retval;

  retval.re() = real - other.re();
  retval.im() = ima  - other.im();

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator-= (goComplex<T>& other) {
  real -= other.re();
  ima  -= other.im();
  info.argValid = false;
  info.absValid = false;

  return *this;
}

template <class T>
goComplex<T>
goComplex<T>::operator/ (goComplex& other) {
  goComplex<T> retval;
  T tmp;

  tmp = (other.re()*other.re()) + (other.im()*other.im());
  retval.re() = ( (real * other.re()) + (ima * other.im()) ) / tmp; 
  retval.im() = ( (other.re() * ima) - (real * other.im()) ) / tmp;
  info.argValid = false;
  info.absValid = false;

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator/= (goComplex<T>& other) {
  T tmpreal;
  T tmpima;
  T tmp;

  tmp		= (other.re()*other.re()) + (other.im()*other.im());
  tmpreal	= ( (real * other.re()) + (ima * other.im()) ) / tmp; 
  tmpima	= ( (other.re() * ima) - (real * other.im()) ) / tmp;

  real = tmpreal;
  ima  = tmpima;
  info.argValid = false;
  info.absValid = false;
  return *this;
}

template <class T>
goComplex<T>&
goComplex<T>::operator= (goComplex& other) {
  real = other.re();
  ima  = other.im();
  info.argValid = false;
  info.absValid = false;
  return *this;
}

template <class T>
bool
goComplex<T>::operator== (goComplex<T>& other) {
  if ( (other.re() == real) && (other.im() == ima) ) {
    return true;
  }
  return false;
}

template <class T>
bool
goComplex<T>::operator!= (goComplex<T>& other) {
  if ( (other.re() != real) || (other.im() != ima) ) {
    return true;
  }
  return false;
}

template <class T>
void
goComplex<T>::con () {
  ima = -ima;
  info.argValid = false;
}

template <class T>
goDouble
goComplex<T>::arg () {
  if (!info.argValid) {
    if (real != 0) { 
      if (real > 0) {
	if (ima >= 0)
	  argValue = ( (T)atan (ima / real) ); 
	else argValue = ( M_PI + M_PI_2 - (T)atan (ima / real) );
      } else {
	if (ima >= 0)
	  argValue = ( M_PI_2 - (T)atan (ima / real) );
	else argValue = ( M_PI + (T)atan (ima / real) );
      }

    } else {
      argValue = (T)(M_PI_2);
    }
    info.argValid = true;
  }
  return argValue;
}

template <class T>
goDouble
goComplex<T>::abs () {
  if (!info.absValid) {
    absValue = (goDouble)sqrt ( (double) (ima*ima + real*real) );
    info.absValid = true;
  }
  return absValue;
}

std::ostream& operator<< (std::ostream& o, goComplex<double>& c) {
  o << c.re() << " + " << c.im() << "i";
  return o;
}

template <class T>
bool
goComplex<T>::
operator> (goComplex<T> &other) {
  if (other.abs() < this->abs()) {
    return true;
  }
  return false;
}

template <class T>
bool
goComplex<T>::
operator< (goComplex<T> &other) {
  if (other.abs() > this->abs()) {
    return true;
  }
  return false;
}

/* Instantiation */
template class goComplex<double>;


