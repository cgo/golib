#ifndef GO3DMACROS_H
#define GO3DMACROS_H

/*!
 * Copies block <CODE>source</CODE> of type <CODE>sourcetype</CODE>
 * to <CODE>target</CODE> of type <CODE>targettype</CODE>.
 * source must fit into target, but they do not need to be of
 * the same size.
 */
#define GO_3D_COPYBLOCK(source,sourcetype,target,targettype) {	\
  goPtrdiff_t __dx = source.getXDiff();					\
  goPtrdiff_t __dy = source.getYDiff();					\
  goPtrdiff_t __dz = source.getZDiff();					\
  goPtrdiff_t __tdx = target.getXDiff();				\
  goPtrdiff_t __tdy = target.getYDiff();				\
  goPtrdiff_t __tdz = target.getZDiff();  				\
									\
  goSize_t __x = source.getSizeX();					\
  goSize_t __y = source.getSizeY();					\
  goSize_t __z = source.getSizeZ();					\
									\
  sourcetype *p  = source.getPtr (0,0,0);				\
  targettype *tp = target.getPtr (0,0,0);				\
  sourcetype *py = p;							\
  sourcetype *pz = p;							\
  targettype *tpy = tp;							\
  targettype *tpz = tp;							\
  goSize_t __i,__j,__k;							\
  for (__k = 0; __k < __z; __k++)					\
    {									\
      py = pz;								\
      tpy = tpz;							\
      for (__j = 0; __j < __y; __j++)					\
	{								\
	  p = py;							\
	  tp = tpy;							\
	  for (__i = 0; __i < __x; __i++)				\
	    {								\
	      *tp = *p;							\
	      tp += __tdx;						\
	      p  += __dx;						\
	    }								\
	  py  += __dy;							\
	  tpy += __tdy;							\
	}								\
      pz += __dz;							\
      tpz += __tdz;							\
    }									\
}


#endif
