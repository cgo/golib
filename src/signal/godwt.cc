#include <godwt.h>
#include <gosignal3d.h>
#include <gosubsignal3d.h>
#include <gosignalmacros.h>
#include <goerror.h>
#include <gotypes.h>

#include <math.h>

template< class T >
goDWT<T>::goDWT()
{
}

template< class T >
goDWT<T>::~goDWT()
{
}

#define HAAR_FILTER_BEGIN(__signal, __signal_target, __signal_t, __signal_target_t) {	\
  __signal_t *__ptr_z		= __signal.getPtr();			\
  __signal_t *__ptr = __ptr_z;				\
  __signal_t *__ptr_y = __ptr_z;				\
  __signal_target_t *__ptr_z_target   = __signal_target.getPtr();	\
  __signal_target_t *__ptr_target = __ptr_z_target;			\
  __signal_target_t *__ptr_y_target = __ptr_z_target;			\
  goPtrdiff_t __dx	= __signal.getXDiff();			\
  goPtrdiff_t __dy	= __signal.getYDiff();			\
  goPtrdiff_t __dz	= __signal.getZDiff();			\
  goPtrdiff_t __dx_target	= __signal_target.getXDiff();	\
  goPtrdiff_t __dy_target	= __signal_target.getYDiff();	\
  goPtrdiff_t __dz_target	= __signal_target.getZDiff();	\
  goSize_t __i, __j, __k;					\
  godwt_t  __tmp1, __tmp2;


#define HAAR_FILTER_BEGIN_INPLACE(__signal, __signal_t) {	\
  __signal_t *__ptr_z		= __signal.getPtr();			\
  __signal_t *__ptr = __ptr_z;				\
  __signal_t *__ptr_y = __ptr_z;				\
  goPtrdiff_t __dx	= __signal.getXDiff();			\
  goPtrdiff_t __dy	= __signal.getYDiff();			\
  goPtrdiff_t __dz	= __signal.getZDiff();			\
  goSize_t __i, __j, __k;					\
  godwt_t  __tmp1, __tmp2;


#define HAAR_FILTER_REINIT(__signal,__signal_target) {				\
  __ptr_z_target = __signal_target.getPtr();	\
  __ptr_z	 = __signal.getPtr();				\
  __ptr = __ptr_z; \
  __ptr_y = __ptr_z; \
  __ptr_target = __ptr_z_target; \
  __ptr_y_target = __ptr_z_target; \
}

#define HAAR_FILTER_X(__tp0, __tp1) {							\
  for (__i = 0; __i < signal.getSizeZ(); __i++)						\
    {											\
      __ptr_y_target = __ptr_z_target;							\
      __ptr_y = __ptr_z;								\
      for (__j = 0; __j < signal.getSizeY(); __j++)					\
	{										\
	  __ptr = __ptr_y;								\
	  __ptr_target = __ptr_y_target;						\
	  for (__k = 0; __k < (signal.getSizeX() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr * __tp0;			\
	      __tmp2 = *(__ptr + __dx) * __tp1;		\
	      *__ptr_target = __tmp1 + __tmp2;		\
	      __ptr_target += __dx_target;						\
              *__ptr_target = __tmp1 - __tmp2;		\
	      __ptr_target += __dx_target;					\
	      __ptr += __dx << 1;							\
	    }										\
	  __ptr_y_target += __dy_target;						\
	  __ptr_y += __dy;								\
	}										\
      __ptr_z_target += __dz_target;							\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_Y(__tp0, __tp1) {							\
  for (__i = 0; __i < signal.getSizeZ(); __i++)						\
    {											\
      __ptr_target = __ptr_z_target;							\
      __ptr = __ptr_z;								\
      for (__j = 0; __j < signal.getSizeX(); __j++)					\
	{										\
	  __ptr_y = __ptr;								\
	  __ptr_y_target = __ptr_target;						\
	  for (__k = 0; __k < (signal.getSizeY() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_y * __tp0;			\
	      __tmp2 = *(__ptr_y + __dy) * __tp1;		\
	      *__ptr_y_target = __tmp1 + __tmp2;		\
	      __ptr_y_target += __dy_target;						\
              *__ptr_y_target = __tmp1 - __tmp2;		\
	      __ptr_y_target += __dy_target;					\
	      __ptr_y += __dy << 1;							\
	    }										\
	  __ptr_target += __dx_target;						\
	  __ptr += __dx;								\
	}										\
      __ptr_z_target += __dz_target;							\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_Z(__tp0, __tp1) {							\
  for (__i = 0; __i < signal.getSizeX(); __i++)						\
    {											\
      __ptr_y_target = __ptr_target;							\
      __ptr_y = __ptr;								\
      for (__j = 0; __j < signal.getSizeY(); __j++)					\
	{										\
	  __ptr_z = __ptr_y;								\
	  __ptr_z_target = __ptr_y_target;						\
	  for (__k = 0; __k < (signal.getSizeZ() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_z * __tp0;			\
	      __tmp2 = *(__ptr_z + __dz) * __tp1;		\
	      *__ptr_z_target = __tmp1 + __tmp2;		\
	      __ptr_z_target += __dz_target;						\
              *__ptr_z_target = __tmp1 - __tmp2;		\
	      __ptr_z_target += __dz_target;					\
	      __ptr_z += __dz << 1;							\
	    }										\
	  __ptr_y_target += __dy_target;						\
	  __ptr_y += __dy;								\
	}										\
      __ptr_target += __dx_target;							\
      __ptr += __dx;									\
    }											\
}


#define HAAR_FILTER_X_INPLACE(__tp0, __tp1) {							\
  for (__i = 0; __i < signal.getSizeZ(); __i++)						\
    {											\
      __ptr_y = __ptr_z;								\
      for (__j = 0; __j < signal.getSizeY(); __j++)					\
	{										\
	  __ptr = __ptr_y;								\
	  for (__k = 0; __k < (signal.getSizeX() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr * __tp0;			\
	      __tmp2 = *(__ptr + __dx) * __tp1;		\
	      *__ptr = __tmp1 + __tmp2;		\
	      __ptr += __dx;						\
              *__ptr = __tmp1 - __tmp2;		\
	      __ptr += __dx;					\
	    }										\
	  __ptr_y += __dy;								\
	}										\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_Y_INPLACE(__tp0, __tp1) {							\
  for (__i = 0; __i < signal.getSizeZ(); __i++)						\
    {											\
      __ptr = __ptr_z;								\
      for (__j = 0; __j < signal.getSizeX(); __j++)					\
	{										\
	  __ptr_y = __ptr;								\
	  for (__k = 0; __k < (signal.getSizeY() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_y * __tp0;			\
	      __tmp2 = *(__ptr_y + __dy) * __tp1;		\
	      *__ptr_y = __tmp1 + __tmp2;		\
	      __ptr_y += __dy;						\
              *__ptr_y = __tmp1 - __tmp2;		\
	      __ptr_y += __dy;					\
	    }										\
	  __ptr += __dx;								\
	}										\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_Z_INPLACE(__tp0, __tp1) {							\
  for (__i = 0; __i < signal.getSizeX(); __i++)						\
    {											\
      __ptr_y = __ptr;								\
      for (__j = 0; __j < signal.getSizeY(); __j++)					\
	{										\
	  __ptr_z = __ptr_y;								\
	  for (__k = 0; __k < (signal.getSizeZ() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_z * __tp0;			\
	      __tmp2 = *(__ptr_z + __dz) * __tp1;		\
	      *__ptr_z = __tmp1 + __tmp2;		\
	      __ptr_z += __dz;						\
              *__ptr_z = __tmp1 - __tmp2;		\
	      __ptr_z += __dz;					\
	    }										\
	  __ptr_y += __dy;								\
	}										\
      __ptr += __dx;									\
    }											\
}


#define HAAR_FILTER_REVERSE_BEGIN(__tp0, __tp1) {	\
  godwt_t __f0, __f1;					\
  __f0 = 1 / (2 * __tp0);				\
  __f1 = 1 / (2 * __tp1);

#define HAAR_FILTER_REVERSE_END() }

// Assumes that tp0 == tp1. Just to save 2 multiplications. True for HAAR filters.
#define HAAR_FILTER_REVERSE_X(__sig, __target_type) {					    \
  for (__i = 0; __i < __sig.getSizeZ(); __i++)						\
    {											\
      __ptr_y_target = __ptr_z_target;							\
      __ptr_y = __ptr_z;								\
      for (__j = 0; __j < __sig.getSizeY(); __j++)					\
	{										\
	  __ptr = __ptr_y;								\
	  __ptr_target = __ptr_y_target;						\
	  for (__k = 0; __k < (__sig.getSizeX() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr * __f0;			\
	      __tmp2 = *(__ptr + __dx) * __f0;		\
	      *__ptr_target = (__target_type)(__tmp1 + __tmp2);		\
	      __ptr_target += __dx_target;						\
              *__ptr_target = (__target_type)(__tmp1 - __tmp2);		\
	      __ptr_target += __dx_target;					\
	      __ptr += __dx << 1;							\
	    }										\
	  __ptr_y_target += __dy_target;						\
	  __ptr_y += __dy;								\
	}										\
      __ptr_z_target += __dz_target;							\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_REVERSE_Y(__sig) {					   \
  for (__i = 0; __i < __sig.getSizeZ(); __i++)						\
    {											\
      __ptr_target = __ptr_z_target;							\
      __ptr = __ptr_z;								\
      for (__j = 0; __j < __sig.getSizeX(); __j++)					\
	{										\
	  __ptr_y = __ptr;								\
	  __ptr_y_target = __ptr_target;						\
	  for (__k = 0; __k < (__sig.getSizeY() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_y * __f0;			\
	      __tmp2 = *(__ptr_y + __dy) * __f0;		\
	      *__ptr_y_target = __tmp1 + __tmp2;		\
	      __ptr_y_target += __dy_target;						\
              *__ptr_y_target = __tmp1 - __tmp2;		\
	      __ptr_y_target += __dy_target;					\
	      __ptr_y += __dy << 1;							\
	    }										\
	  __ptr_target += __dx_target;						\
	  __ptr += __dx;								\
	}										\
      __ptr_z_target += __dz_target;							\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_REVERSE_Z(__sig) {					     \
  for (__i = 0; __i < __sig.getSizeX(); __i++)						\
    {											\
      __ptr_y_target = __ptr_target;							\
      __ptr_y = __ptr;								\
      for (__j = 0; __j < __sig.getSizeY(); __j++)					\
	{										\
	  __ptr_z = __ptr_y;								\
	  __ptr_z_target = __ptr_y_target;						\
	  for (__k = 0; __k < (__sig.getSizeZ() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_z * __f0;			\
	      __tmp2 = *(__ptr_z + __dz) * __f0;		\
	      *__ptr_z_target = __tmp1 + __tmp2;		\
	      __ptr_z_target += __dz_target;						\
              *__ptr_z_target = __tmp1 - __tmp2;		\
	      __ptr_z_target += __dz_target;					\
	      __ptr_z += __dz << 1;							\
	    }										\
	  __ptr_y_target += __dy_target;						\
	  __ptr_y += __dy;								\
	}										\
      __ptr_target += __dx_target;							\
      __ptr += __dx;									\
    }											\
}


#define HAAR_FILTER_REVERSE_X_INPLACE(__sig) {					    \
  for (__i = 0; __i < __sig.getSizeZ(); __i++)						\
    {											\
      __ptr_y = __ptr_z;								\
      for (__j = 0; __j < __sig.getSizeY(); __j++)					\
	{										\
	  __ptr = __ptr_y;								\
	  for (__k = 0; __k < (__sig.getSizeX() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr * __f0;			\
	      __tmp2 = *(__ptr + __dx) * __f0;		\
	      *__ptr = __tmp1 + __tmp2;		\
	      __ptr += __dx;						\
              *__ptr = __tmp1 - __tmp2;		\
	      __ptr += __dx;					\
	    }										\
	  __ptr_y += __dy;								\
	}										\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_REVERSE_Y_INPLACE(__sig) {					   \
  for (__i = 0; __i < __sig.getSizeZ(); __i++)						\
    {											\
      __ptr = __ptr_z;								\
      for (__j = 0; __j < __sig.getSizeX(); __j++)					\
	{										\
	  __ptr_y = __ptr;								\
	  for (__k = 0; __k < (__sig.getSizeY() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_y * __f0;			\
	      __tmp2 = *(__ptr_y + __dy) * __f0;		\
	      *__ptr_y = __tmp1 + __tmp2;		\
	      __ptr_y += __dy;						\
              *__ptr_y = __tmp1 - __tmp2;		\
	      __ptr_y += __dy;					\
	    }										\
	  __ptr += __dx;								\
	}										\
      __ptr_z += __dz;									\
    }											\
}

#define HAAR_FILTER_REVERSE_Z_INPLACE(__sig) {					     \
  for (__i = 0; __i < __sig.getSizeX(); __i++)						\
    {											\
      __ptr_y = __ptr;								\
      for (__j = 0; __j < __sig.getSizeY(); __j++)					\
	{										\
	  __ptr_z = __ptr_y;								\
	  for (__k = 0; __k < (__sig.getSizeZ() >> 1); __k++)   /* one less than size! */	\
	    {										\
	      __tmp1 = *__ptr_z * __f0;			\
	      __tmp2 = *(__ptr_z + __dz) * __f0;		\
	      *__ptr_z = __tmp1 + __tmp2;		\
	      __ptr_z += __dz;						\
              *__ptr_z = __tmp1 - __tmp2;		\
	      __ptr_z += __dz;					\
	    }										\
	  __ptr_y += __dy;								\
	}										\
      __ptr += __dx;									\
    }											\
}


#define HAAR_FILTER_END() }


inline
static
void
F1 (goSignal3D<godwt_t>& input_signal, goSignal3D<godwt_t>& output_signal)
{
  
}

template<class T>
void
goDWT<T>::haar(goSignal3D<T>& signal, goSignal3D<godwt_t>& targetSignal)
{
  // goDouble tp[] = {1,1};
  // goDouble hp[] = {1,-1};
  goSignal3D<godwt_t>	temp1;  // 
  goSignal3D<godwt_t>	temp2;  
  temp1.make (signal.getSizeX(), signal.getSizeY(), signal.getSizeZ(),
	      0, 0, 0);
  temp2.make (signal.getSizeX(), signal.getSizeY(), signal.getSizeZ(),
	      0, 0, 0);
  
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  HAAR_FILTER_BEGIN(signal,temp1, T, godwt_t);
  // temp1.setSizeX(temp1.getSizeX() >> 1);
  // temp1.setXDiff(temp1.getXDiff() << 1);
  HAAR_FILTER_X(tp0,tp1);
  HAAR_FILTER_END();  

  /*
   * temp1: L H L H L H ....
   *        L H L H L H ....
   */

  HAAR_FILTER_BEGIN(temp1,temp2,godwt_t, godwt_t);
  HAAR_FILTER_Y(tp0,tp1);
  HAAR_FILTER_END();

  /*
   * temp2: LL HL ...
   *        LH HH ...
   *        .....
   */
  
  HAAR_FILTER_BEGIN(temp2,targetSignal,godwt_t, godwt_t);
  HAAR_FILTER_Z(tp0,tp1);
  HAAR_FILTER_END();

  /*
   * targetSignal:    .
   *                .
   *         LLL HLL ....
   *         LHL HHL ....
   *         ......
   */

  temp1.destroy();
  temp2.destroy();
}

// template<class T>
// void
// goDWT<T>::haar (goSignal3D<T>& signal)
// {
//   goError::note("goDWT::haar()","Not implemented for this data type. Please use haar(goSignal3D, goSignal3D)");
//   exit(2);
// }

void
goDWT<goFloat>::haar(goSignal3D<goFloat>& signal)
{
  // goDouble tp[] = {1,1};
  // goDouble hp[] = {1,-1};
  
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  HAAR_FILTER_BEGIN_INPLACE(signal, goFloat);
  // temp1.setSizeX(temp1.getSizeX() >> 1);
  // temp1.setXDiff(temp1.getXDiff() << 1);
  HAAR_FILTER_X_INPLACE(tp0,tp1);
  HAAR_FILTER_END();  

  /*
   * temp1: L H L H L H ....
   *        L H L H L H ....
   */

  // HAAR_FILTER_BEGIN(temp1,temp2,godwt_t, godwt_t);
  HAAR_FILTER_BEGIN_INPLACE(signal, goFloat);
  HAAR_FILTER_Y_INPLACE(tp0,tp1);
  HAAR_FILTER_END();

  /*
   * temp2: LL HL ...
   *        LH HH ...
   *        .....
   */
  
  // HAAR_FILTER_BEGIN(temp2,targetSignal,godwt_t, godwt_t);
  HAAR_FILTER_BEGIN_INPLACE(signal, goFloat);
  HAAR_FILTER_Z_INPLACE(tp0,tp1);
  HAAR_FILTER_END();

  /*
   * targetSignal:    .
   *                .
   *         LLL HLL ....
   *         LHL HHL ....
   *         ......
   */
}

void
goDWT<goDouble>::haar(goSignal3D<goDouble>& signal)
{
  // goDouble tp[] = {1,1};
  // goDouble hp[] = {1,-1};
  
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  HAAR_FILTER_BEGIN_INPLACE(signal, goDouble);
  // temp1.setSizeX(temp1.getSizeX() >> 1);
  // temp1.setXDiff(temp1.getXDiff() << 1);
  HAAR_FILTER_X_INPLACE(tp0,tp1);
  HAAR_FILTER_END();  

  /*
   * temp1: L H L H L H ....
   *        L H L H L H ....
   */

  // HAAR_FILTER_BEGIN(temp1,temp2,godwt_t, godwt_t);
  HAAR_FILTER_BEGIN_INPLACE(signal, goDouble);
  HAAR_FILTER_Y_INPLACE(tp0,tp1);
  HAAR_FILTER_END();

  /*
   * temp2: LL HL ...
   *        LH HH ...
   *        .....
   */
  
  // HAAR_FILTER_BEGIN(temp2,targetSignal,godwt_t, godwt_t);
  HAAR_FILTER_BEGIN_INPLACE(signal, goDouble);
  HAAR_FILTER_Z_INPLACE(tp0,tp1);
  HAAR_FILTER_END();

  /*
   * targetSignal:    .
   *                .
   *         LLL HLL ....
   *         LHL HHL ....
   *         ......
   */
}


template< class T >
void
goDWT<T>::unHaar (goSignal3D<godwt_t>& haarSignal, goSignal3D<T>& targetSignal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  //  goDouble hp0 = tp0;
  //  goDouble hp1 = -hp0;
  goSignal3D<godwt_t>	temp1;
  goSignal3D<godwt_t>	temp2;
  temp1.make (haarSignal.getSizeX(), haarSignal.getSizeY(), haarSignal.getSizeZ(),
	      0, 0, 0);
  temp2.make (haarSignal.getSizeX(), haarSignal.getSizeY(), haarSignal.getSizeZ(),
	      0, 0, 0);

  HAAR_FILTER_BEGIN(haarSignal, temp1, godwt_t, godwt_t);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_Z(haarSignal);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

  HAAR_FILTER_BEGIN(temp1, temp2, godwt_t, godwt_t);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_Y(temp1);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

  HAAR_FILTER_BEGIN(temp2, targetSignal, godwt_t, T);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_X(targetSignal,T);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

  temp1.destroy();
  temp2.destroy();
} 


// template<class T>
// void
// goDWT<T>::unHaar (goSignal3D<T>& haarSignal)
// {
//   goError::note("goDWT::unHaar()","Not implemented for this data type. Please use unHaar(goSignal3D, goSignal3D)");  
//   exit(2);
// }

void
goDWT<goFloat>::unHaar(goSignal3D<goFloat>& haarSignal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  //  goDouble hp0 = tp0;
  //  goDouble hp1 = -hp0;

  HAAR_FILTER_BEGIN_INPLACE(haarSignal, goFloat);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_Z_INPLACE(haarSignal);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

  HAAR_FILTER_BEGIN_INPLACE(haarSignal, goFloat);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_Y_INPLACE(haarSignal);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

  HAAR_FILTER_BEGIN_INPLACE(haarSignal, goFloat);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_X_INPLACE(haarSignal);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

} 

void
goDWT<goDouble>::unHaar (goSignal3D<goDouble>& haarSignal)
{
  goDouble tp0 = 0.5;
  goDouble tp1 = tp0;
  
  //  goDouble hp0 = tp0;
  //  goDouble hp1 = -hp0;

  HAAR_FILTER_BEGIN_INPLACE(haarSignal, goDouble);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_Z_INPLACE(haarSignal);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

  HAAR_FILTER_BEGIN_INPLACE(haarSignal, goDouble);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_Y_INPLACE(haarSignal);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

  HAAR_FILTER_BEGIN_INPLACE(haarSignal, goDouble);
  HAAR_FILTER_REVERSE_BEGIN(tp0, tp1);
  HAAR_FILTER_REVERSE_X_INPLACE(haarSignal);
  HAAR_FILTER_REVERSE_END();
  HAAR_FILTER_END();

} 


template< class T >
int
goDWT<T>::haar(goSignal3D<T>& signal, int stage)
{
  int i;
  goSubSignal3D<T>  s;
  s.setParent(&signal);
  s.setSize (signal.getSizeX(),
	     signal.getSizeY(),
	     signal.getSizeZ());
  s.setDiff (signal.getXDiff(),
	     signal.getYDiff(),
	     signal.getZDiff());
  for (i = 0; i < stage; i++)
    {
      haar (s);
      s.shiftRightSize(1);
      s.shiftLeftDiff(1);
    }
  return stage;
}

template< class T >
int
goDWT<T>::unHaar(goSignal3D<T>& signal, int stage)
{
  int i;
  goSubSignal3D<T>  s;
  s.setParent(&signal);
  s.setSize (signal.getSizeX() >> (stage - 1),
	     signal.getSizeY() >> (stage - 1),
	     signal.getSizeZ() >> (stage - 1));
  s.setDiff (signal.getXDiff() << (stage - 1),
	     signal.getYDiff() << (stage - 1),
	     signal.getZDiff() << (stage - 1));
  for (i = 0; i < stage; i++)
    {
      unHaar (s);
      s.shiftLeftSize(1);
      s.shiftRightDiff(1);
    }
  return stage;
}

/***********************************************************************************/
/* Specialised integer routines follow						   */
/***********************************************************************************/

/*******************************************************/
/* This code is not to be disclosed to the public.     */
/* For the used macros, the public probably doesn't	   */
/* want to see it anyway ;-)						   */
/*******************************************************/

#define ST_XY_BEGIN(__block, __T) {\
  __T *p;\
  goSize_t x = __block.getSizeX();\
  goSize_t y = __block.getSizeY();\
/*  goSize_t z = __block.getSizeZ(); */\
  goPtrdiff_t dx = __block.getXDiff();\
  goPtrdiff_t dy = __block.getYDiff();\
  goPtrdiff_t dz = __block.getZDiff();\
  register __T r1,r2,r3,r4,r5,r6,r7,r8;\
  register __T *p1;\
  __T *p2;\
  __T *p3;\
  __T *p4;\
  __T *p5;\
  __T *p6;\
  __T *p7;\
  __T *p8;\
  register goSize_t i,j;\
  __T tmp1, tmp2, tmp3, tmp4;

#define ST_XY_END() }

/* Use 32 bit for storage of the
 * transform. Unfortunately, the modified S-Transform implies this.
 * S Transform a slice.
 * Call format:
 * STSlice (go3DBlock<GO_STRANSFORM_IN_T> *block, 
 *          go3DBlock<GO_STRANSFORM_OUT_T> *newBlock, 
 *          goIndex_t slice);
 */
#define ST_XY(__block, __slice) { \
  p = __block.getPtr();\
  p += __slice * dz;\
\
  for (i = y; i > 0; i -= 2) {\
    p1 = p;\
    p2 = p1 + dx;\
    p3 = p1 + dy;\
    p4 = p3 + dx;\
    for (j = x; j > 0; j -= 2) {\
      r1 = *p1 + *(p1 + dy);\
      r2 = *(p1 + dx + dy) + *(p1 + dx);\
      /* division by 2: integer shift */\
      r3 = r1 >> 1;\
      r4 = r2 >> 1;\
\
      /* S(m,n) */\
      tmp1 = (r3 + r4) >> 1;\
      /* S(m+1,n) */\
      tmp2 = r3 - r4;\
      /* S(m,n+1) */\
      tmp3 = (*p1 - *(p1 + dy) - *(p1 + dx + dy) + *(p1 + dx)) >> 1;\
      /* S(m+1,n+1) */\
      tmp4 = *p1 - *(p1 + dy) + *(p1 + dx + dy) - *(p1 + dx);\
\
      *p1 = tmp1;\
      *p2 = tmp2;\
      *p3 = tmp3;\
      *p4 = tmp4;\
      \
      p1 += dx << 1;\
      p2 += dx << 1;\
      p3 += dx << 1;\
      p4 += dx << 1;\
    }\
    p += dy << 1;\
  }\
}

#define ST_REVERSE_XY_BEGIN(__block, __T) {\
  __T *p = __block.getPtr();\
  goSize_t x = __block.getSizeX();\
  goSize_t y = __block.getSizeY();\
/*  goSize_t z = __block.getSizeZ(); */\
  goPtrdiff_t dx = __block.getXDiff();\
  goPtrdiff_t dy = __block.getYDiff();\
  register goPtrdiff_t dx_2 = dx << 1;\
  register goPtrdiff_t dy_2 = dy << 1;\
\
  goSize_t i,j;\
  register __T a_,b_,a,b,A_,B_,A,B;\
  __T r1,r2,r3,r4,r5,r6,r7,r8;\
  __T a1,a2,a3,a4,a5,a6,a7,a8;\
  __T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8;

#define ST_REVERSE_XY_END() }

/* Reverse transform a slice
 * Call format:
 * TSSlice (go3DBlock<GO_STRANSFORM_OUT_T> *block, 
 *          go3DBlock<GO_STRANSFORM_IN_T> *newBlock, 
 *          goIndex_t slice);
 */
#define ST_REVERSE_XY(__block, __slice) {\
  p = __block.getPtr();\
  p += __slice * __block.getZDiff();\
  \
  /* OUT_T tmp1,tmp2,tmp3,tmp4; */\
  for (i = (y >> 1); i > 0; i--) {\
    p1 = p;\
/*      p2 = p1 + ( (x >> 1) * dx ); */\
/*      p3 = p1 + ( (y >> 1) * dy ); */\
/*      p4 = p3 + ( (x >> 1) * dx ); */\
    p2 = p1 + dx;\
    p3 = p1 + dy;\
    p4 = p3 + dx;\
    for (j = (x >> 1); j > 0; j--) {\
      A_ = *p1;\
      B_ = *p2;\
      A  = *p3;\
      B  = *p4;\
      a_ = A_ - ( (-B_) >> 1 );\
      b_ = a_ - B_;\
      a  = A - ( (-B) >> 1 );\
      b  = a - B;\
\
      /* *newP1			= a_ - ( (-a) >> 1 ); */\
      A				= a_ - ( (-a) >> 1 );\
      *p1 = A;\
      *(p1 + dy)		= A - a;\
      /* *(newP1 + newdx)	= b_ - ( (-b) >> 1 ); */\
      B				= b_ - ( (-b) >> 1 );\
      *(p1 + dx) = B;\
      *(p1 + dx + dy)	= B - b;\
      \
      \
/*        p1	+= dx; */\
/*        p2	+= dx; */\
/*        p3	+= dx; */\
/*        p4	+= dx; */\
      p1	+= dx_2;\
      p2	+= dx_2;\
      p3	+= dx_2;\
      p4	+= dx_2;\
    }\
    /* p    += dy; */\
    p += dy_2;\
  }\
}


/*
 * STZ and TSZ are done in-place since they use the same data type for
 * input and output. 
 * The data organisation in {ST|TS}Slice is as described in go3ddwt.h.
 * The coder has to step through the data
 * not in 1-steps but in 2-steps with an appropriate offset for each 
 * subband.
 */
#define ST_Z(STZ_block, __T) {\
  __T *STZ_p	= STZ_block.getPtr();\
\
  goSize_t STZ_x = STZ_block.getSizeX();\
  goSize_t STZ_y = STZ_block.getSizeY();\
  goSize_t STZ_z = STZ_block.getSizeZ();\
\
  register goPtrdiff_t STZ_dx = STZ_block.getXDiff();\
  register goPtrdiff_t STZ_dy = STZ_block.getYDiff();\
  goPtrdiff_t STZ_dz = STZ_block.getZDiff();\
\
  register goIndex_t STZ_i,STZ_j,STZ_k;\
  /* register OUT_T r1,r2,r3,r4,r5,r6,r7,r8; */\
  /* register OUT_T a1,a2,a3,a4,a5,a6,a7,a8; */\
  /* OUT_T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8; */\
  __T *pSave = STZ_p;\
  for (STZ_k = (STZ_z >> 1); STZ_k > 0; STZ_k--) {\
    STZ_p = pSave;\
    for (STZ_j = (STZ_y >> 1); STZ_j > 0; STZ_j--) {\
      p1 = STZ_p;\
      p2 = p1 + STZ_dz;\
      p3 = p1 + STZ_dx;\
      p4 = p3 + STZ_dz;\
      p5 = p1 + STZ_dy;\
      p6 = p5 + STZ_dz;\
      p7 = p3 + STZ_dy;\
      p8 = p7 + STZ_dz;\
/*        newP2 = newP1 + (newdz * (z >> 1)); */\
/*        newP3 = newP1 + (newdx * (x >> 1)); */\
/*        newP4 = newP3 + (newdz * (z >> 1)); */\
/*        newP5 = newP1 + (newdy * (y >> 1)); */\
/*        newP6 = newP5 + (newdz * (z >> 1)); */\
/*        newP7 = newP3 + (newdy * (y >> 1)); */\
/*        newP8 = newP7 + (newdz * (z >> 1)); */\
      for (STZ_i = (STZ_x >> 1); STZ_i > 0; STZ_i--) {\
	r1 = *p1;\
	r2 = *p2;\
	r3 = *p3;\
	r4 = *p4;\
	r5 = *p5;\
	r6 = *p6;\
	r7 = *p7;\
	r8 = *p8;\
\
	*p1 = (r1 + r2) >> 1;\
	*p2 = r1 - r2;\
	*p3 = (r3 + r4) >> 1;\
	*p4 = r3 - r4;\
	*p5 = (r5 + r6) >> 1;\
	*p6 = r5 - r6;\
	*p7 = (r7 + r8) >> 1;\
	*p8 = r7 - r8;\
\
	p1 += STZ_dx << 1;\
	p2 += STZ_dx << 1;\
	p3 += STZ_dx << 1;\
	p4 += STZ_dx << 1;\
	p5 += STZ_dx << 1;\
	p6 += STZ_dx << 1;\
	p7 += STZ_dx << 1;\
	p8 += STZ_dx << 1;\
      }\
      STZ_p += STZ_dy << 1;\
    }\
    pSave += STZ_dz << 1;\
  }\
}


#define ST_REVERSE_Z(TSZ_block, __T) {\
  __T *TSZ_p = TSZ_block.getPtr();\
\
  goSize_t TSZ_x = TSZ_block.getSizeX();\
  goSize_t TSZ_y = TSZ_block.getSizeY();\
  goSize_t TSZ_z = TSZ_block.getSizeZ();\
\
  register goPtrdiff_t TSZ_dx = TSZ_block.getXDiff();\
  register goPtrdiff_t TSZ_dy = TSZ_block.getYDiff();\
  goPtrdiff_t TSZ_dz = TSZ_block.getZDiff();\
\
  register goPtrdiff_t TSZ_dx_2 = TSZ_dx << 1;\
  register goPtrdiff_t TSZ_dy_2 = TSZ_dy << 1;\
  goPtrdiff_t TSZ_dz_2 = TSZ_dz << 1;\
\
  register goIndex_t TSZ_i,TSZ_j,TSZ_k;\
  /* register OUT_T r1,r2,r3,r4,r5,r6,r7,r8; */ \
  /* register OUT_T a1,a2,a3,a4,a5,a6,a7,a8; */ \
  /* OUT_T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8; */ \
  __T *pSave = TSZ_p;\
  \
  for (TSZ_k = (TSZ_z >> 1); TSZ_k > 0; TSZ_k--) {\
    TSZ_p = pSave;\
    for (TSZ_j = (TSZ_y >> 1); TSZ_j > 0; TSZ_j--) {\
      p1 = TSZ_p;\
      p2 = p1 + TSZ_dz;\
      p3 = p1 + TSZ_dx;\
      p4 = p3 + TSZ_dz;\
      p5 = p1 + TSZ_dy;\
      p6 = p5 + TSZ_dz;\
      p7 = p3 + TSZ_dy;\
      p8 = p7 + TSZ_dz;\
/*        p2 = p1 + (dz * (z >> 1)); */\
/*        p3 = p1 + (dx * (x >> 1)); */\
/*        p4 = p3 + (dz * (z >> 1)); */\
/*        p5 = p1 + (dy * (y >> 1)); */\
/*        p6 = p5 + (dz * (z >> 1)); */\
/*        p7 = p3 + (dy * (y >> 1)); */\
/*        p8 = p7 + (dz * (z >> 1)); */\
      for (TSZ_i = (TSZ_x >> 1); TSZ_i > 0; TSZ_i--) {\
	r1 = *p1;\
	r2 = *p2;\
	r3 = *p3;\
	r4 = *p4;\
	r5 = *p5;\
	r6 = *p6;\
	r7 = *p7;\
	r8 = *p8;\
	\
	a1 = (r1 - ( (-r2) >> 1 ));\
	a2 = (a1 - r2);\
	a3 = (r3 - ( (-r4) >> 1 ));\
	a4 = (a3 - r4);\
	a5 = (r5 - ( (-r6) >> 1 ));\
	a6 = (a5 - r6);\
	a7 = (r7 - ( (-r8) >> 1 ));\
	a8 = (a7 - r8);\
	*p1 = a1;\
	*p2 = a2;\
	*p3 = a3;\
	*p4 = a4;\
	*p5 = a5;\
	*p6 = a6;\
	*p7 = a7;\
	*p8 = a8;\
\
	p1 += TSZ_dx_2;\
	p2 += TSZ_dx_2;\
	p3 += TSZ_dx_2;\
	p4 += TSZ_dx_2;\
	p5 += TSZ_dx_2;\
	p6 += TSZ_dx_2;\
	p7 += TSZ_dx_2;\
	p8 += TSZ_dx_2;\
      }\
      TSZ_p += TSZ_dy_2;\
    }\
    pSave += TSZ_dz_2;\
  }\
}


/* This method uses not exactly a Haar transform, but rather an enhanced
 * S Transform using only integers, additions, and shifts, which makes it faster
 * than a floating point transform on some machines.
 */											
#define GO_DWT_INTEGER_HAAR_METHOD(__TYPE)						 \
void											 \
goDWT<__TYPE>::haar (goSignal3D<__TYPE> &signal) {					 \
  /* 2D ST of each xy slice */								 \
  goIndex_t i_st;									 \
  ST_XY_BEGIN(signal, __TYPE)								 \
  for (i_st = 0; i_st < (goIndex_t)signal.getSizeZ(); i_st++) 				 \
    {											 \
      ST_XY (signal, i_st);								 \
    }											 \
  /* 1D ST in z direction */								 \
  ST_Z (signal, __TYPE);								 \
  ST_XY_END()										 \
}

#define GO_DWT_INTEGER_UNHAAR_METHOD(__TYPE)					\
void										\
goDWT<__TYPE>::unHaar (goSignal3D<__TYPE> &signal) {				\
  goIndex_t i_ts;								\
  ST_REVERSE_XY_BEGIN(signal, __TYPE);						\
  /* 1D reverse ST in z direction */						\
  ST_REVERSE_Z (signal, __TYPE);  						\
  for (i_ts = 0; i_ts < (goIndex_t)signal.getSizeZ(); i_ts++) {			\
    ST_REVERSE_XY (signal, i_ts);						\
  }										\
  ST_REVERSE_XY_END()								\
}


/*
 * Yes, I know this is bad style. Yes, I COULD handle it differently.
 * BUT I want to keep it this way in case this class will be needed for some special types 
 * and I have to do some specialised member.
 * Unfortunately, my c++ compiler apparently does not handle inline templates.
 */
GO_DWT_INTEGER_HAAR_METHOD(goInt8)
GO_DWT_INTEGER_HAAR_METHOD(goUInt8)
GO_DWT_INTEGER_HAAR_METHOD(goInt16)
GO_DWT_INTEGER_HAAR_METHOD(goUInt16)
GO_DWT_INTEGER_HAAR_METHOD(goInt32)
GO_DWT_INTEGER_HAAR_METHOD(goUInt32)

GO_DWT_INTEGER_UNHAAR_METHOD(goInt8)
GO_DWT_INTEGER_UNHAAR_METHOD(goUInt8)
GO_DWT_INTEGER_UNHAAR_METHOD(goInt16)
GO_DWT_INTEGER_UNHAAR_METHOD(goUInt16)
GO_DWT_INTEGER_UNHAAR_METHOD(goInt32)
GO_DWT_INTEGER_UNHAAR_METHOD(goUInt32)

/*
 * Static function STZ is used to perform the transform along the 3rd axis.
 * <CODE>STSlice()</CODE> and <CODE>STZ()</CODE> should be merged to 
 * enhance the performance.
 */
// int
// goDWT<goInt16>::haar (goSignal3D<goInt16> &signal, int stage) {
//   /* This method uses not exactly a Haar transform, but rather an enhanced
//    * S Transform using only integers, additions, and shifts, which makes it faster
//    * than a floating point transform on some machines. 
//    */

//   /* 2D ST of each xy slice */
//   goIndex_t i_st;
//   ST_XY_BEGIN(signal, goInt16)
//   for (i_st = 0; i_st < (goIndex_t)signal.getSizeZ(); i_st++) 
//     {
//       ST_XY (signal, i_st);
//     }
//   /* 1D ST in z direction */
//   ST_Z (signal, goInt16);
//   ST_XY_END()
  
//   return stage;
// }

/*
 * Reverse of <CODE>haar(integer_type)</CODE>.
 * See <CODE>haar(integer_type)</CODE> for comments.
 */
// int
// goDWT<goInt16>::unHaar (goSignal3D<goInt16> &signal, int stage) {
//   goIndex_t i_ts;
//   ST_REVERSE_XY_BEGIN(signal, goInt16);
//   /* 1D reverse ST in z direction */
//   ST_REVERSE_Z (signal, goInt16);  
//   for (i_ts = 0; i_ts < (goIndex_t)signal.getSizeZ(); i_ts++) {
//     ST_REVERSE_XY (signal, i_ts);
//   }
//   ST_REVERSE_XY_END()
//   return stage;
// }


template class goDWT<goInt8>;
template class goDWT<goUInt8>;
template class goDWT<goInt16>;
template class goDWT<goUInt16>;
template class goDWT<goInt32>;
template class goDWT<goUInt32>;
template class goDWT<goFloat>;
template class goDWT<goDouble>;
