#ifndef GO3DALGO_H
#define GO3DALGO_H

#include <gotypes.h>
#include <go3vector.h>

/* static inline void clipLiangBarsky (goDouble xmin, goDouble xmax, goDouble ymin,  */
/* 				    goDouble ymax, goDouble zmin, goDouble zmax, */
/* 				    go3Vector<goDouble>& d, go3Vector<goDouble>& p1, */
/* 				    go3Vector<goDouble>* start,  */
/* 				    go3Vector<goDouble>* end); */

/*!
 * Liang-Barsky 3D line clipping.
 * @param d The direction vector of length of the whole line (NOT unity!)
 * @param p1 Start point of the line to be clipped
 * @param xmin ... zmax: Min/max values on the 3 main axes of the box against which the line is to be clipped
 * @param start Vector containing the start point of the clipped line after clipping (relative to p1)
 * @param end Vector containing the end point of the clipped line after clipping (relative to p1)
 */
static inline void clipLiangBarsky (goDouble xmin, goDouble xmax, goDouble ymin, 
				    goDouble ymax, goDouble zmin, goDouble zmax,
				    go3Vector<goDouble>& d, go3Vector<goDouble>& p1,
				    go3Vector<goDouble>* start, 
				    go3Vector<goDouble>* end)
{
  goDouble p[6];
  goDouble q[6];
  p[0] = -d.x;
  p[1] = d.x;
  p[2] = -d.y;
  p[3] = d.y;
  p[4] = -d.z;
  p[5] = d.z;

  q[0] = p1.x - xmin;
  q[1] = xmax - p1.x;
  q[2] = p1.y - ymin;
  q[3] = ymax - p1.y;
  q[4] = p1.z - zmin;
  q[5] = zmax - p1.z;

  // Calculate qi/pi \forall i 
  goSize_t i;
  goDouble m1 = 0.0f, m2 = 1.0f, mT = 0.0f;
  for (i = 0; i < 6; i++)
    {
      if (p[i] != 0)
	{
	  mT = q[i] / p[i];
	}
      // Check for parallel lines outside the box
      if (p[i] == 0)
	{
	  if (q[i] < 0)
	    {
	      return;
	    }
	} else
	  {
	    // Calculate fraction and compare with previous results to get min/max values
	    // cout << "q/p = " << mT << endl;
	    if (p[i] < 0)
	      {
		// cout << "p" << i << " < 0, m1 = " << m1 << endl;
		if (mT > m1)
		  m1 = mT;
	      } else
		{
		  if (p[i] > 0)
		    {
		      // cout << "p" << i << " > 0, m2 = " << m2 << endl;
		      if (mT < m2)
			{
			  m2 = mT;
			}
		    }
		}
	  }
    }
  // We should now have the start point (m1 * line vector)
  // and end point (m2 * line vector) of the clipped line.
  // Use the data members directly, that's lots faster than
  // using *= and = operators.
  if (m1 < m2)
    {
      m1 = m2;
    }
  start->x = d.x * m1;
  start->y = d.y * m1;
  start->z = d.z * m1;
  end->x = d.x * m2;
  end->y = d.y * m2;
  end->z = d.z * m2;
  // cout << "m1 = " << m1 << ", m2 = " << m2 << endl;
}

#define GO_3D_CLIPLIANGBARSKY_START() {		\
  goDouble __p[6];				\
  goDouble __q[6];				\
  goSize_t __i;					\
  goDouble __m1, __m2, __mT = 0.0f;

#define GO_3D_CLIPLIANGBARSKY_END() }

// Same as the above Liang Barsky function as macro.
// If you have a lot of clipping to do, this saves the calls and
// some push operations.
// __isVisible is a bool variable and set by the macro; 
//   true = line is valid, false = forget the line
#define GO_3D_CLIPLIANGBARSKY(__xmin, __xmax, __ymin, 	\
			      __ymax, __zmin, __zmax,	\
			      __d, __p1,		\
			      __start, __end,__isVisible)		\
{							\
  __p[0] = -__d.x;					\
  __p[1] = __d.x;					\
  __p[2] = -__d.y;					\
  __p[3] = __d.y;					\
  __p[4] = -__d.z;					\
  __p[5] = __d.z;					\
							\
  __q[0] = __p1.x - __xmin;				\
  __q[1] = __xmax - __p1.x;				\
  __q[2] = __p1.y - __ymin;				\
  __q[3] = __ymax - __p1.y;				\
  __q[4] = __p1.z - __zmin;				\
  __q[5] = __zmax - __p1.z;				\
							\
  __m1 = 0.0f; __m2 = 1.0f;				\
  for (__i = 0; __i < 6; __i++)				\
    {							\
      if (__p[__i] != 0)				\
	{						\
	  __mT = __q[__i] / __p[__i];			\
	}						\
      if (__p[__i] == 0)				\
	{						\
	  if (__q[__i] < 0)				\
	    {						\
              __m1 = __m2;				\
	      break;					\
	    }						\
/*                cout << "p" << __i << " = 0, NO break" << endl; \ */ \
	} else						\
	  {						\
	    if (__p[__i] < 0)				\
	      {						\
		if (__mT > __m1)			\
		  __m1 = __mT;				\
	      } else					\
		{					\
		  if (__p[__i] > 0)			\
		    {					\
		      if (__mT < __m2)			\
			{				\
			  __m2 = __mT;			\
			}				\
		    }					\
		}					\
	  }						\
    }							\
  if (__m1 < __m2)					\
    {							\
      __isVisible = true;				\
      __start.x = __d.x * __m1;				\
      __start.y = __d.y * __m1;				\
      __start.z = __d.z * __m1;				\
      __end.x = __d.x * __m2;				\
      __end.y = __d.y * __m2;				\
      __end.z = __d.z * __m2;				\
    } else {						\
      __isVisible = false;				\
    }							\
}


#endif







