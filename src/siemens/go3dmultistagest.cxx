#include <go3dmultistagest.h>
#include <goerror.h>

template< class T,class OUT_T >
go3DMultiStageST<T,OUT_T>::go3DMultiStageST () {
  init (0);
}

template< class T,class OUT_T >
go3DMultiStageST<T,OUT_T>::~go3DMultiStageST () {
}

template< class T,class OUT_T >
void
go3DMultiStageST<T,OUT_T>::init (void*) {
  stages	= 1;
  block		= 0;
  transform	= 0;
}

/*!
 * Copies block <CODE>source</CODE> of type <CODE>sourcetype</CODE>
 * to <CODE>target</CODE> of type <CODE>targettype</CODE>.
 * source must fit into target, but they do not need to be of
 * the same size.
 */
#define GO_MSST_COPYBLOCK(source,sourcetype,target,targettype) {	\
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
  
#define DEBUG_OUT() {							    \
  goError::note ("go3DMultiStageST::reverse()","Size of trans: "); \
  cout << "\t" << trans.getSizeX() << "," << trans.getSizeY() << "," << trans.getSizeZ() << endl;	    \
  goError::note ("go3DMultiStageST::reverse()","Diff of trans: "); \
  cout << "\t" << trans.getXDiff() << "," << trans.getYDiff() << "," << trans.getZDiff() << endl;			    \
  goError::note ("go3DMultiStageST::reverse()","Size of outBlock: "); \
  cout << "\t" << outBlock.getSizeX() << "," << outBlock.getSizeY() << "," << outBlock.getSizeZ() << endl;	    \
  goError::note ("go3DMultiStageST::reverse()","Diff of outBlock: "); \
  cout << "\t" << outBlock.getXDiff() << "," << outBlock.getYDiff() << "," << outBlock.getZDiff() << endl;			    \
}

template< class T,class OUT_T >
void
go3DMultiStageST<T,OUT_T>::execute () {		
  goSize_t s;					
  go3DBlock<OUT_T>	outBlock;
  go3DBlock<OUT_T>	trans;
  go3DBlock<OUT_T>	temp;
  dwt1.set3DBlock (block);
  dwt1.setSTransform (transform);
  dwt1.st();
  outBlock.make (block->getSizeX() >> 1,
		 block->getSizeY() >> 1,
		 block->getSizeZ() >> 1);
  trans    = *transform;

  goSize_t xsize = trans.getSizeX() >> 1;
  goSize_t ysize = trans.getSizeY() >> 1;
  goSize_t zsize = trans.getSizeZ() >> 1;
  goPtrdiff_t dx = trans.getXDiff() << 1;
  goPtrdiff_t dy = trans.getYDiff() << 1;
  goPtrdiff_t dz = trans.getZDiff() << 1;
  dwt2.setSTransform (&trans);
  dwt2.set3DBlock (&outBlock);
  for (s = 1; s < stages; s++) 			
    {
      trans.setSize (xsize,
		     ysize,
		     zsize);
      trans.setDiff (dx, dy, dz);
//        DEBUG_OUT();
      GO_MSST_COPYBLOCK(trans,OUT_T,outBlock,OUT_T);
      dwt2.st();
      /***************************************************************/
      // Quantise ... not.
//        goSize_t xs,ys,zs;
//        OUT_T tempT;
//        goDouble tempDouble;
//        goDouble delta = 16;
//        for (zs = 0; zs < outBlock.getSizeZ(); zs++)
//  	{
//  	  for (ys = 0; ys < outBlock.getSizeY(); ys++)
//  	    {
//  	      for (xs = 0; xs < outBlock.getSizeX(); xs++)
//  		{
//  		  tempT = *outBlock.getPtr(xs,ys,zs);
//  		  tempDouble = (abs(tempT) + delta) / (float)(2*delta + 1);
//  		  if (tempT < 0)
//  		    tempT = -(T)tempDouble;
//  		  else tempT = (T)tempDouble;
//  		  *outBlock.getPtr(xs,ys,zs) = tempT;
//  		}
//  	    }
//  	}
      /***************************************************************/

      outBlock.setSize (outBlock.getSizeX() >> 1,
			outBlock.getSizeY() >> 1,
			outBlock.getSizeZ() >> 1);
      outBlock.setDiff (outBlock.getXDiff() << 1,
			outBlock.getYDiff() << 1,
			outBlock.getZDiff() << 1);
      dx = dx << 1;
      dy = dy << 1;
      dz = dz << 1;
      xsize = xsize >> 1;
      ysize = ysize >> 1;
      zsize = zsize >> 1;
    }
  outBlock.setSize (block->getSizeX() >> 1,
		    block->getSizeY() >> 1,
		    block->getSizeZ() >> 1);
  outBlock.destroy();
}


template< class T,class OUT_T >
void
go3DMultiStageST<T,OUT_T>::reverse (goSize_t reconStages) {		
  goSize_t s;					
  go3DBlock<OUT_T>		outBlock;
  go3DBlock<OUT_T>		trans;
  trans = *transform;
  outBlock.make (trans.getSizeX(),
		 trans.getSizeY(),
		 trans.getSizeZ());
  outBlock.setDiff (outBlock.getXDiff() << (stages - 1),
		    outBlock.getYDiff() << (stages - 1),
		    outBlock.getZDiff() << (stages - 1));
  goSize_t xsize = trans.getSizeX() >> (stages - 1);
  goSize_t ysize = trans.getSizeY() >> (stages - 1);
  goSize_t zsize = trans.getSizeZ() >> (stages - 1);
  goPtrdiff_t dx = trans.getXDiff() << (stages - 1);
  goPtrdiff_t dy = trans.getYDiff() << (stages - 1);
  goPtrdiff_t dz = trans.getZDiff() << (stages - 1);
  dwt2.setSTransform (&trans);
  dwt2.set3DBlock (&outBlock);
  for (s = 1; s < reconStages; s++) 			
    {
      outBlock.setSize (xsize, ysize, zsize);
      trans.setSize (xsize, ysize, zsize);
      trans.setDiff (dx, dy, dz);
//        DEBUG_OUT();
      dwt2.ts();
      GO_MSST_COPYBLOCK(outBlock,OUT_T,trans,OUT_T);
      xsize = xsize << 1;
      ysize = ysize << 1;
      zsize = zsize << 1;
      dx = dx >> 1;
      dy = dy >> 1;
      dz = dz >> 1;
      outBlock.setDiff (outBlock.getXDiff() >> 1,
			outBlock.getYDiff() >> 1,
			outBlock.getZDiff() >> 1);
    }
  /* 
   * funktioniert nur bei vollständiger Rücktransformation !
   * AAAAAAH!
   */
  /* FixThisShit(TM) */
  trans.setSize (xsize,ysize,zsize);
  trans.setDiff (dx,dy,dz);
  dwt1.setSTransform (&trans);
  // dwt1.setSTransform (transform);
  dwt1.set3DBlock (block);
  dwt1.ts();
  
  outBlock.setSize (block->getSizeX(),
		    block->getSizeY(),
		    block->getSizeZ());
  outBlock.destroy();
}

template class go3DMultiStageST< goInt8, goInt32 >;
template class go3DMultiStageST< goUInt8, goInt32 >;
template class go3DMultiStageST< goInt16, goInt32 >;





