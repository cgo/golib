#ifndef GOVOLUMERENDERERMACROS_H
#define GOVOLUMERENDERERMACROS_H

#include <govol.h>

/*-------------------------------------------------------------------*/
/* Macros from govolumerendererexp */

#define GO_VREXP_FINDBOUNDS_START() {\
   go4Vector<volFloat> __pos;

#define GO_VREXP_FINDBOUNDS_END() }

#define GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down) { \
  if (__pos.x < __left)				\
    {						\
      __left = __pos.x;				\
    } else					\
      {						\
        if (__pos.x > __right)			\
        {					\
          __right = __pos.x;			\
        }					\
      }						\
  if (__pos.y < __up)				\
    {						\
      __up = __pos.y;				\
    } else					\
      {						\
        if (__pos.y > __down)			\
        {					\
          __down = __pos.y;			\
        }					\
      }						\
}



#define GO_VREXP_FINDBOUNDS(__blockStart, \
			    __left, __right, \
			    __up, __down) {	\
  __pos		= __blockStart;			\
  __pos		*= Tproj;			\
  __pos.div();					\
  __left	= __pos.x;			\
  __right	= __left;			\
  __up		= __pos.y;			\
  __down	= __up;				\
						\
  __pos = __blockStart;				\
  __pos.x += xStep;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.y += yStep;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.x += xStep;				\
  __pos.y += yStep;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep;				\
  __pos.x += xStep;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep;				\
  __pos.y += yStep;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += zStep;				\
  __pos.x += xStep;				\
  __pos.y += yStep;				\
  __pos *= Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
}

#define GO_VREXP_FINDBOUNDS_2(__blockStart, \
			    __left, __right, \
			    __up, __down, __xStep, __yStep, __zStep, __Tproj) {	\
  __pos		= __blockStart;			\
  __pos		*= __Tproj;			\
  __pos.div();					\
  __left	= __pos.x;			\
  __right	= __left;			\
  __up		= __pos.y;			\
  __down	= __up;				\
						\
  __pos = __blockStart;				\
  __pos.x += __xStep;				\
  __pos *= __Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.y += __yStep;				\
  __pos *= __Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.x += __xStep;				\
  __pos.y += __yStep;				\
  __pos *= __Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += __zStep;				\
  __pos *= __Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += __zStep;				\
  __pos.x += __xStep;				\
  __pos *= __Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += __zStep;				\
  __pos.y += __yStep;				\
  __pos *= __Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
  __pos = __blockStart;				\
  __pos.z += __zStep;				\
  __pos.x += __xStep;				\
  __pos.y += __yStep;				\
  __pos *= __Tproj;				\
  __pos.div();					\
  GO_VREXP_FINDBOUNDS_COMPARE(__left,__right,__up,__down);\
}

#endif
