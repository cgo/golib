#ifndef __GO_COMPLEX_H__
#define __GO_COMPLEX_H__

#include <iostream>
#include <gotypes.h>

/*!
 * \addtogroup math
 * @{
 */
/**
 * @brief Complex number class.
 **/
template <class T>
class goComplex {
 public:
  goComplex  ();
  goComplex  (T r, T i);
  goComplex  (const goComplex& other);
  ~goComplex ();

  inline T&	re () { return real; }
  inline T&	im () { return ima; }
  inline const T&	re () const { return real; }
  inline const T&	im () const { return ima; }

  goComplex<T>	operator*  (const goComplex<T>& other);
  goComplex<T>&	operator*= (const goComplex<T>& other);
  goComplex<T>	operator+  (const goComplex<T>& other);
  goComplex<T>&	operator+= (const goComplex<T>& other);
  goComplex<T>	operator-  (const goComplex<T>& other);
  goComplex<T>&	operator-= (const goComplex<T>& other);
  goComplex<T>	operator/  (const goComplex<T>& other);
  goComplex<T>&	operator/= (const goComplex<T>& other);
  goComplex<T>&	operator=  (const goComplex<T>& other);
  bool		operator== (const goComplex<T>& other) const;
  bool		operator!= (const goComplex<T>& other) const;
  bool		operator>  (const goComplex<T>& other) const;
  bool		operator<  (const goComplex<T>& other) const;

  /// make conjugate complex number
  void	conjugate ();

  /**
   * Calculate the argument (phase angle) of the complex number.
   * @returns Argument of this complex number.
   */
  goDouble  arg () const;

  /**
   * Calculate the absolute value of the complex number.
   * @returns Absolute value of this complex number
   */
  goDouble  abs () const;

protected:
  T real;
  T ima;
};
/*! @} */
///
std::ostream& operator<< (std::ostream& o, class goComplex<goDouble>& c);

#endif 



