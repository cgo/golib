#include <go3ddwt.h>
#include <goerror.h>


template< class T,class OUT_T >
go3DDWT<T,OUT_T>::go3DDWT () {
  block = 0;
  sTransform = 0;
  sTransformDeletable = true;
}

template< class T,class OUT_T >
go3DDWT<T,OUT_T>::~go3DDWT () {
  if (sTransform && sTransformDeletable) {
    sTransform->destroy();
    delete sTransform;
  }
}

template< class T,class OUT_T >
void
go3DDWT<T,OUT_T>::set3DBlock (go3DBlock<T> *b) {
  block = b;
  if (sTransform && sTransformDeletable) {
    sTransform->destroy();
    delete sTransform;
    sTransform = 0;
  }
}

template< class T,class OUT_T >
go3DBlock<OUT_T>*
go3DDWT<T,OUT_T>::dwt () {
  /* Use implementation in original golib. */
  return NULL;
}

#define STSLICE_BEGIN(__block, __newBlock) {\
  T *p;\
  OUT_T *newP;\
  goSize_t x = __block->getSizeX();\
  goSize_t y = __block->getSizeY();\
  goSize_t z = __block->getSizeZ();\
  goPtrdiff_t dx = __block->getXDiff();\
  goPtrdiff_t dy = __block->getYDiff();\
  goPtrdiff_t dz = __block->getZDiff();\
  goPtrdiff_t newdx = __newBlock->getXDiff();\
  goPtrdiff_t newdy = __newBlock->getYDiff();\
  goPtrdiff_t newdz = __newBlock->getZDiff();\
  register OUT_T r1,r2,r3,r4,r5,r6,r7,r8;\
  register T *p1;\
  register OUT_T *newP1;\
  OUT_T *newP2;\
  OUT_T *newP3;\
  OUT_T *newP4;\
  OUT_T *newP5;\
  OUT_T *newP6;\
  OUT_T *newP7;\
  OUT_T *newP8;\
  register goSize_t i,j;\
  OUT_T tmp1, tmp2, tmp3, tmp4;

#define STSLICE_END() }

/* Use 32 bit for storage of the
 * transform. Unfortunately, the modified S-Transform implies this.
 * S Transform a slice.
 * Call format:
 * STSlice (go3DBlock<GO_STRANSFORM_IN_T> *block, 
 *          go3DBlock<GO_STRANSFORM_OUT_T> *newBlock, 
 *          goIndex_t slice);
 */
#define STSLICE(__block, __newBlock, slice) { \
  p = __block->getPtr();\
  newP = __newBlock->getPtr();\
  p += slice * dz;\
  newP += slice * newdz;\
\
  for (i = y; i > 0; i -= 2) {\
    p1 = p;\
    newP1 = newP;\
/*      newP2 = newP1 + (newdx * (x >> 1)); */\
/*      newP3 = newP1 + (newdy * (y >> 1)); */\
/*      newP4 = newP3 + (newdx * (x >> 1)); */\
    newP2 = newP1 + newdx;\
    newP3 = newP1 + newdy;\
    newP4 = newP3 + newdx;\
    for (j = x; j > 0; j -= 2) {\
      r1 = *p1 + *(p1 + dy);\
      r2 = *(p1 + dx + dy) + *(p1 + dx);\
      /* assume we use integer values! */\
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
      *newP1 = tmp1;\
      *newP2 = tmp2;\
      *newP3 = tmp3;\
      *newP4 = tmp4;\
      \
      p1 += (dx << 1);\
/*        newP1 += newdx; */\
/*        newP2 += newdx; */\
/*        newP3 += newdx; */\
/*        newP4 += newdx; */\
      newP1 += newdx << 1;\
      newP2 += newdx << 1;\
      newP3 += newdx << 1;\
      newP4 += newdx << 1;\
    }\
    /* newP += newdy; */\
    newP += newdy << 1;\
    p += dy << 1;\
  }\
}

#define TSSLICE_BEGIN(__block, __newBlock) {\
  OUT_T *p = __block->getPtr();\
  T *newP = __newBlock->getPtr();\
  goSize_t x = __block->getSizeX();\
  goSize_t y = __block->getSizeY();\
  goSize_t z = __block->getSizeZ();\
  goPtrdiff_t dx = __block->getXDiff();\
  goPtrdiff_t dy = __block->getYDiff();\
  register goPtrdiff_t dx_2 = dx << 1;\
  register goPtrdiff_t dy_2 = dy << 1;\
\
  goPtrdiff_t newdx = __newBlock->getXDiff();\
  goPtrdiff_t newdy = __newBlock->getYDiff();\
  goPtrdiff_t newdz = __newBlock->getZDiff();\
  register goPtrdiff_t newdx_2 = newdx << 1;\
  register goPtrdiff_t newdy_2 = newdy << 1;\
  register goPtrdiff_t newdz_2 = newdz << 1;\
  goSize_t i,j;\
  register OUT_T a_,b_,a,b,A_,B_,A,B;\
  OUT_T r1,r2,r3,r4,r5,r6,r7,r8;\
  OUT_T a1,a2,a3,a4,a5,a6,a7,a8;\
  OUT_T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8;\
  T *newP1;

#define TSSLICE_END() }

/* Reverse transform a slice
 * Call format:
 * TSSlice (go3DBlock<GO_STRANSFORM_OUT_T> *block, 
 *          go3DBlock<GO_STRANSFORM_IN_T> *newBlock, 
 *          goIndex_t slice);
 */
#define TSSLICE(__block, __newBlock, slice) {\
  p = __block->getPtr();\
  newP = __newBlock->getPtr();\
  p += slice * __block->getZDiff();\
  newP += slice * __newBlock->getZDiff();\
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
    newP1 = newP;\
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
      *newP1 = A;\
      *(newP1 + newdy)		= A - a;\
      /* *(newP1 + newdx)	= b_ - ( (-b) >> 1 ); */\
      B				= b_ - ( (-b) >> 1 );\
      *(newP1 + newdx) = B;\
      *(newP1 + newdx + newdy)	= B - b;\
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
      newP1	+= newdx_2;\
    }\
    /* p    += dy; */\
    p += dy_2;\
    newP += newdy_2;\
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
#define STZ(STZ_block) {\
  OUT_T *STZ_p	= STZ_block->getPtr();\
\
  goSize_t STZ_x = STZ_block->getSizeX();\
  goSize_t STZ_y = STZ_block->getSizeY();\
  goSize_t STZ_z = STZ_block->getSizeZ();\
\
  register goPtrdiff_t STZ_dx = STZ_block->getXDiff();\
  register goPtrdiff_t STZ_dy = STZ_block->getYDiff();\
  goPtrdiff_t STZ_dz = STZ_block->getZDiff();\
\
  register goIndex_t STZ_i,STZ_j,STZ_k;\
  /* register OUT_T r1,r2,r3,r4,r5,r6,r7,r8; */\
  /* register OUT_T a1,a2,a3,a4,a5,a6,a7,a8; */\
  /* OUT_T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8; */\
  OUT_T *pSave = STZ_p;\
  for (STZ_k = (STZ_z >> 1); STZ_k > 0; STZ_k--) {\
    STZ_p = pSave;\
    for (STZ_j = (STZ_y >> 1); STZ_j > 0; STZ_j--) {\
      newP1 = STZ_p;\
      newP2 = newP1 + STZ_dz;\
      newP3 = newP1 + STZ_dx;\
      newP4 = newP3 + STZ_dz;\
      newP5 = newP1 + STZ_dy;\
      newP6 = newP5 + STZ_dz;\
      newP7 = newP3 + STZ_dy;\
      newP8 = newP7 + STZ_dz;\
/*        newP2 = newP1 + (newdz * (z >> 1)); */\
/*        newP3 = newP1 + (newdx * (x >> 1)); */\
/*        newP4 = newP3 + (newdz * (z >> 1)); */\
/*        newP5 = newP1 + (newdy * (y >> 1)); */\
/*        newP6 = newP5 + (newdz * (z >> 1)); */\
/*        newP7 = newP3 + (newdy * (y >> 1)); */\
/*        newP8 = newP7 + (newdz * (z >> 1)); */\
      for (STZ_i = (STZ_x >> 1); STZ_i > 0; STZ_i--) {\
	r1 = *newP1;\
	r2 = *newP2;\
	r3 = *newP3;\
	r4 = *newP4;\
	r5 = *newP5;\
	r6 = *newP6;\
	r7 = *newP7;\
	r8 = *newP8;\
\
	*newP1 = (r1 + r2) >> 1;\
	*newP2 = r1 - r2;\
	*newP3 = (r3 + r4) >> 1;\
	*newP4 = r3 - r4;\
	*newP5 = (r5 + r6) >> 1;\
	*newP6 = r5 - r6;\
	*newP7 = (r7 + r8) >> 1;\
	*newP8 = r7 - r8;\
\
	newP1 += STZ_dx << 1;\
	newP2 += STZ_dx << 1;\
	newP3 += STZ_dx << 1;\
	newP4 += STZ_dx << 1;\
	newP5 += STZ_dx << 1;\
	newP6 += STZ_dx << 1;\
	newP7 += STZ_dx << 1;\
	newP8 += STZ_dx << 1;\
      }\
      STZ_p += STZ_dy << 1;\
    }\
    pSave += STZ_dz << 1;\
  }\
}


#define TSZ(TSZ_block) {\
  OUT_T *TSZ_p = TSZ_block->getPtr();\
\
  goSize_t TSZ_x = TSZ_block->getSizeX();\
  goSize_t TSZ_y = TSZ_block->getSizeY();\
  goSize_t TSZ_z = TSZ_block->getSizeZ();\
\
  register goPtrdiff_t TSZ_dx = TSZ_block->getXDiff();\
  register goPtrdiff_t TSZ_dy = TSZ_block->getYDiff();\
  goPtrdiff_t TSZ_dz = TSZ_block->getZDiff();\
\
  register goPtrdiff_t TSZ_dx_2 = TSZ_dx << 1;\
  register goPtrdiff_t TSZ_dy_2 = TSZ_dy << 1;\
  goPtrdiff_t TSZ_dz_2 = TSZ_dz << 1;\
\
  register goIndex_t TSZ_i,TSZ_j,TSZ_k;\
  /* register OUT_T r1,r2,r3,r4,r5,r6,r7,r8; */ \
  /* register OUT_T a1,a2,a3,a4,a5,a6,a7,a8; */ \
  /* OUT_T *p1,*p2,*p3,*p4,*p5,*p6,*p7,*p8; */ \
  OUT_T *pSave = TSZ_p;\
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


/*
 * Static function STZ is used to perform the transform along the 3rd axis.
 * <CODE>STSlice()</CODE> and <CODE>STZ()</CODE> should be merged to 
 * enhance the performance.
 */
template< class T,class OUT_T >
go3DBlock<OUT_T>*
go3DDWT<T,OUT_T>::st () {
  /* S Transform using only integers. */
  if (!block) {
    goError::print ("go3DDWT::st", "block is NULL pointer");
    return NULL;
  }

  // Block to save the transformed data in.
  // go3DBlock *newBlock = new go3DBlock;
  // newBlock->make (block);

  if (!sTransform) {
    sTransform = new go3DBlock<OUT_T>;
    sTransform->make (block->getSizeX(),
		      block->getSizeY(),
		      block->getSizeZ());
  }

  /* 2D ST of each xy slice */
  goIndex_t i_st;
  STSLICE_BEGIN(block, sTransform)
  for (i_st = 0; i_st < (goIndex_t)block->getSizeZ(); i_st++) 
    {
      STSLICE (block, sTransform, i_st);
    }
  /* 1D ST in z direction */
  STZ (sTransform);
  STSLICE_END()
  
  return sTransform;
}

/*
 * Reverse of <CODE>st()</CODE>.
 * See <CODE>st()</CODE> for comments.
 */
template< class T,class OUT_T >
go3DBlock<T>*
go3DDWT<T,OUT_T>::ts () {
  if (!block) {
    goError::print ("go3DDWT::ts()", "block is NULL pointer");
    return NULL;
  }

  if (!sTransform) {
    goError::print ("go3DDWT::ts()","No transform in this object.");
    return NULL;
  }

  goIndex_t i_ts;
  TSSLICE_BEGIN(sTransform, block);
  /* 1D reverse ST in z direction */
  TSZ (sTransform);  
  for (i_ts = 0; i_ts < (goIndex_t)block->getSizeZ(); i_ts++) {
    TSSLICE (sTransform, block, i_ts);
  }
  TSSLICE_END()
  if (sTransformDeletable) {
    sTransform->destroy();
    delete sTransform;
    sTransform = 0;
  }
  return block;
}

template< class T,class OUT_T >
void
go3DDWT<T,OUT_T>::setSTransform (go3DBlock<OUT_T> *s) {
  if (sTransformDeletable && sTransform) {
    sTransform->destroy();
    delete sTransform;
  }
  if (!s) {
    sTransform = 0;
    sTransformDeletable = true;
  } else {
    sTransform = s;
    sTransformDeletable = false;
  }
}

template class go3DDWT< goInt8,goInt32 >;
template class go3DDWT< goUInt8,goInt32 >;
template class go3DDWT< goInt16,goInt32 >;
template class go3DDWT< goInt32,goInt32 >;

