/** \addtogroup signal */
/** @{ */

/*!
 * \file Macros for doing stuff with signals of the goSignal[*] classes.
 */

#ifndef GOSIGNALMACROS_H
#define GOSIGNALMACROS_H

#include <gotypes.h>

/*!
 * Executes <code>__dothis</code> for each element in the <code>goSignal3D</code>
 * <code>__signal</code> of type <code>__type</code>. <br>
 * The signal is went through using the pointer differences 
 * provided by <code>goSignal3D</code>.
 * \author Christian Gosch
 */
#define GO_SIGNAL3D_EACHELEMENT(__dothis, __signal, __type) {	\
  __type *__ptr_z      	= __signal.getPtr(0, 0, 0);			\
  __type *__ptr;						\
  __type *__ptr_y;						\
  const goPtrdiff_t* __dx	= __signal.getXDiff();			\
  const goPtrdiff_t* __dy	= __signal.getYDiff();			\
  const goPtrdiff_t* __dz	= __signal.getZDiff();			\
  goSize_t __i,__j,__k;						\
  \
  for (__i = 0; __i < __signal.getSizeZ(); __i++)		\
    {								\
      __ptr_y = __ptr_z;					\
      for (__j = 0; __j < __signal.getSizeY(); __j++)		\
	{							\
	  __ptr = __ptr_y;					\
	  for (__k = 0; __k < __signal.getSizeX(); __k++)	\
	    {							\
	      {							\
		    __dothis;					\
		    __ptr += __dx[__k];					\
	      }							\
	    }							\
	  __ptr_y += __dy[__j];					\
	}							\
      __ptr_z += __dz[__i];						\
    }								\
								\
}


/*!
 * Does the same as <code>GO_SIGNAL3D_EACHELEMENT</code>
 * but provides walking through 2 signals of the same size.
 * That can be used to store the results in a second signal.
 */
#define GO_SIGNAL3D_EACHELEMENT_2(__dothis, __signal, __signal_target, __type, __type_target) {	\
  __type *__ptr_z		= __signal.getPtr();		\
  __type *__ptr_y;						\
  __type *__ptr;						\
  __type_target *__ptr_z_target   = __signal_target.getPtr();	\
  __type_target *__ptr_y_target;				\
  __type_target *__ptr_target;					\
  const goPtrdiff_t* __dx	= __signal.getXDiff();			\
  const goPtrdiff_t* __dy	= __signal.getYDiff();			\
  const goPtrdiff_t* __dz	= __signal.getZDiff();			\
  const goPtrdiff_t* __dx_target	= __signal_target.getXDiff();	\
  const goPtrdiff_t* __dy_target	= __signal_target.getYDiff();	\
  const goPtrdiff_t* __dz_target	= __signal_target.getZDiff();	\
  goSize_t __i, __j, __k;					\
  for (__i = 0; __i < __signal.getSizeZ(); ++__i)		\
    {								\
      __ptr_y = __ptr_z;					\
      __ptr_y_target = __ptr_z_target;				\
      for (__j = 0; __j < __signal.getSizeY(); ++__j)     	\
	{							\
	  __ptr = __ptr_y;					\
	  __ptr_target = __ptr_y_target;			\
	  for (__k = 0; __k < __signal.getSizeX(); ++__k)	\
	    {							\
	      {							\
		    __dothis;					\
		    __ptr += __dx[__k];					\
		    __ptr_target += __dx_target[__k];			\
	      }							\
	    }							\
	  __ptr_y += __dy[__j];					\
	  __ptr_y_target += __dy_target[__j];			\
	}							\
      __ptr_z += __dz[__i];						\
      __ptr_z_target += __dz_target[__i];				\
    }								\
}

/** @} */

#endif



