#ifndef GOINTEGRAL_H
#define GOINTEGRAL_H

#include <goarray.h>

struct goIntegralStatus {
  /// True if the value is computed and valid
  bool value_valid;
};

/*
 * This class seems unused and will be dumped.
 *
 * This implements some numerical integration methods.
 * Methods implemented so far are:
 * \begin{itemize}
 *  Trapezoid integration
 * \end{itemize}
 */
template <class T>
class goIntegral {
 public:
  ///
  goIntegral ();
  ///
  ~goIntegral ();
  
  ///
  enum GO_INTEGRAL_TYPE {
    GO_INTEGRAL_TRAPEZ
  };
  
  /// Sets the function values and the distance (x-axis) between two values
  void	setFunction	(goArray<T>& v, T dist);
  /// Sets the distance between two function values.
  void  setDistance	(T dist) { distance = dist; }
  /// Sets a pointer to a function value array. The array has to be deleted by the user.
  void	setFunctionPtr	(goArray<T>* v);
  ///
  void	setType		(GO_INTEGRAL_TYPE t) { type = t; } 
  ///
  GO_INTEGRAL_TYPE getType () { return type; }
  /// Evaluates the integral value. 
  T	eval		();
protected:
  /// Function values
  goArray<T>*		function;
  ///
  GO_INTEGRAL_TYPE	type;
  ///
  goIntegralStatus	status;
  ///
  T			value;
  ///
  T			distance;
  ///
  bool			delete_function;
};

#endif /* GOINTEGRAL_H */
