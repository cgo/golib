#ifndef __GORESAMPLE_H
#define __GORESAMPLE_H

#include <gotypes.h>
#include <gosignal2d.h>

template <class T>
class goResample {
 public:
  goResample (goInt32 factor = 2);
  goResample (goInt32 factorX, goInt32 factorY);
  virtual ~goResample ();
  
  /// Set the resampling factor.
  void		setM (goInt32 mx, goInt32 my) { MX = mx; MY = my; }
  /// @return Resampling factor in x direction.
  goInt32	getMX () { return MX; }
  /// @return Resampling factor in y direction.
  goInt32	getMY () { return MY; }

  ///
  void		down (goSignal2D<T>& src, goSignal2D<T>& target);

  ///
  void		up (goSignal2D<T>& src, goSignal2D<T>& target);

 protected:
  goInt32 MX;
  goInt32 MY;
};

#endif

