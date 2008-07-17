#ifndef __GORESAMPLE_H
#define __GORESAMPLE_H

#include <gotypes.h>
#ifndef GOMATH_H
# include <gomath.h>
#endif

namespace goMath
{
    template <class T>
        bool resampleCubic (const goMatrix<T>& source, goMatrix<T>& target, goSize_t resamplePointCount, bool closed = false, goFixedArray<goDouble>* accumLength_ = 0);

    template <class T>
        void resampleLinear (const goFixedArray<T>& f, goFixedArray<T>& ret);
};

template <class T>
bool goResampleLinear (const goMatrix<T>& source, goMatrix<T>& target, goSize_t resamplePointCount);

//= Check where this class is used if at all, if not, REMOVE THIS CLASS
//= Used in the filterbank/reverse classes. Remove all this from 0.5.
#if 0
template <class T>
class goResample {
 public:
  goResample (goInt32 factor = 2);
  goResample (goInt32 factorX, goInt32 factorY);
  virtual ~goResample ();
  
  // Set the resampling factor.
  void		setM (goInt32 mx, goInt32 my) { MX = mx; MY = my; }
  // @return Resampling factor in x direction.
  goInt32	getMX () { return MX; }
  // @return Resampling factor in y direction.
  goInt32	getMY () { return MY; }

  void		down (goSignal2D<T>& src, goSignal2D<T>& target);

  void		up (goSignal2D<T>& src, goSignal2D<T>& target);

 protected:
  goInt32 MX;
  goInt32 MY;
};
#endif

#endif
