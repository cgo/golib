#ifndef __GO_COMPLEX_H__
#define __GO_COMPLEX_H__

#include <iostream>
#include <gotypes.h>

class goComplexInfo {
public:
  bool	absValid;
  bool	argValid;
};

template <class T>
class goComplex {
 public:
  goComplex  ();
  goComplex  (T r, T i);
  goComplex  (goComplex& other);
  ~goComplex ();

  inline T&	re () { return real; }
  inline T&	im () { return ima; }

  goComplex<T>	operator*  (goComplex<T>& other);
  goComplex<T>&	operator*= (goComplex<T>& other);
  goComplex<T>	operator+  (goComplex<T>& other);
  goComplex<T>&	operator+= (goComplex<T>& other);
  goComplex<T>	operator-  (goComplex<T>& other);
  goComplex<T>&	operator-= (goComplex<T>& other);
  goComplex<T>	operator/  (goComplex<T>& other);
  goComplex<T>&	operator/= (goComplex<T>& other);
  goComplex<T>&	operator=  (goComplex<T>& other);
  bool		operator== (goComplex<T>& other);
  bool		operator!= (goComplex<T>& other);
  bool		operator> (goComplex<T>& other);
  bool		operator< (goComplex<T>& other);

  /// make conjugate complex number
  void	con ();

  /**
   * Calculate the argument (phase angle) of the complex number.
   * @returns Argument of this complex number.
   */
  goDouble  arg ();

  /**
   * Calculate the absolute value of the complex number.
   * @returns Absolute value of this complex number
   */
  goDouble  abs ();

protected:
  T real;
  T ima;
  goDouble absValue;
  goDouble argValue;
  goComplexInfo	info;
};

///
std::ostream& operator<< (std::ostream& o, class goComplex<goDouble>& c);

#endif 



