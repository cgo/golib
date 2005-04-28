#include <gocomplex.h>
#include <math.h>
#include <gotypes.h>

template <class T>
goComplex<T>::goComplex () {
  real          = 0;
  ima           = 0;
}

template <class T>
goComplex<T>::goComplex (T r, T i) {
  real          = r;
  ima           = i;
}

template <class T>
goComplex<T>::goComplex (const goComplex<T>& other) {
  real          = other.re ();
  ima           = other.im ();
}

template <class T>
goComplex<T>::~goComplex () {
}

template <class T>
goComplex<T>
goComplex<T>::operator* (const goComplex<T>& other) {
  goComplex<T> retval;

  retval.re() = (real * other.re()) - (ima * other.im());
  retval.im() = (real * other.im()) + (ima * other.re());

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator*= (const goComplex<T>& other) {
  T tmpreal;
  T tmpima;

  tmpreal = (real * other.re()) - (ima * other.im());
  tmpima  = (real * other.im()) + (ima * other.re());

  real = tmpreal;
  ima  = tmpima;
  return *this;
}

template <class T>
goComplex<T>
goComplex<T>::operator+ (const goComplex<T>& other) {
  goComplex<T> retval;

  retval.re() = real + other.re();
  retval.im() = ima  + other.im();

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator+= (const goComplex<T>& other) {
  real += other.re();
  ima  += other.im();
  return *this;
}

template <class T>
goComplex<T>
goComplex<T>::operator- (const goComplex<T>& other) {
  goComplex<T> retval;

  retval.re() = real - other.re();
  retval.im() = ima  - other.im();

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator-= (const goComplex<T>& other) {
  real -= other.re();
  ima  -= other.im();

  return *this;
}

template <class T>
goComplex<T>
goComplex<T>::operator/ (const goComplex& other) {
  goComplex<T> retval;
  T tmp;

  tmp = (other.re()*other.re()) + (other.im()*other.im());
  retval.re() = ( (real * other.re()) + (ima * other.im()) ) / tmp; 
  retval.im() = ( (other.re() * ima) - (real * other.im()) ) / tmp;

  return retval;
}

template <class T>
goComplex<T>&
goComplex<T>::operator/= (const goComplex<T>& other) {
  T tmpreal;
  T tmpima;
  T tmp;

  tmp		= (other.re()*other.re()) + (other.im()*other.im());
  tmpreal	= ( (real * other.re()) + (ima * other.im()) ) / tmp; 
  tmpima	= ( (other.re() * ima) - (real * other.im()) ) / tmp;

  real = tmpreal;
  ima  = tmpima;
  return *this;
}

template <class T>
goComplex<T>&
goComplex<T>::operator= (const goComplex& other) {
  real = other.re();
  ima  = other.im();
  return *this;
}

template <class T>
bool
goComplex<T>::operator== (const goComplex<T>& other) const
{
    if ( (other.re() == real) && (other.im() == ima) ) 
    {
        return true;
    }
    return false;
}

template <class T>
bool
goComplex<T>::operator!= (const goComplex<T>& other) const
{
    if ( (other.re() != real) || (other.im() != ima) ) 
    {
        return true;
    }
    return false;
}

template <class T>
void
goComplex<T>::conjugate () {
  ima = -ima;
}

template <class T>
goDouble goComplex<T>::arg () const
{
    goDouble argValue = 0.0;
    if (real != 0) 
    { 
        if (real > 0) 
        {
            if (ima >= 0)
                argValue = ( (T)atan (ima / real) ); 
            else argValue = ( M_PI + M_PI_2 - (T)atan (ima / real) );
        } 
        else 
        {
            if (ima >= 0)
                argValue = ( M_PI_2 - (T)atan (ima / real) );
            else 
                argValue = ( M_PI + (T)atan (ima / real) );
        }

    } 
    else 
    {
        argValue = (T)(M_PI_2);
    }
    return argValue;
}

template <class T>
goDouble
goComplex<T>::abs () const 
{
    return sqrt ( (double) (ima*ima + real*real) );
}

std::ostream& operator<< (std::ostream& o, goComplex<double>& c) {
  o << c.re() << " + " << c.im() << "i";
  return o;
}

template <class T>
bool
goComplex<T>::
operator> (const goComplex<T> &other) const 
{
  if (other.abs() < this->abs()) {
    return true;
  }
  return false;
}

template <class T>
bool
goComplex<T>::
operator< (const goComplex<T> &other) const 
{
  if (other.abs() > this->abs()) {
    return true;
  }
  return false;
}

/* Instantiation */
template class goComplex<double>;
template class goComplex<float>;
