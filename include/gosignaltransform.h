#ifndef GOSIGNALTRANSFORM_H
#define GOSIGNALTRANSFORM_H

#include <gotypes.h>
#include <gosignal.h>
#include <gocomplex.h>

class goSignalTransformInfo {
 public:
  bool	transformValid;
  bool	last_failed;
};

/**
 * Signal transformation. This class is intended to perform transformations
 * like DFT on signals of type goSignal.
 * @author Christian Gosch
 * @see goSignal goNVector goArray
 */
template <class T>
class goSignalTransform : public goSignal<T> {
 public:
  ///
  goSignalTransform ();
  ///
  virtual ~goSignalTransform ();
  
  /**
   * Used to retrieve the transformed signal of type goSignal.
   * The transformed signal is always of type goComplex<goDouble>.
   * This method does not trigger any calculation !
   * @return Void pointer to the transformed goSignal. Cast it to goSignal<goComplex<goDouble> >*.
   * @see goSignal
   * @see goComplex
   */
  void*		getTransform ();  
  
  /**
   * Performs a discrete Fourier transform on the signal.
   * @return True if operation was successful
   */
  bool			dft ();
  
 protected:
  goSignal<goComplex<goDouble> >       *transform;
  goSignalTransformInfo info;
};

#endif











