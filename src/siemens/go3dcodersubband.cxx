#include <go3dcodersubband.h>
#include <go3dsubblock.h>
#include <godefs.h>

template< class T >
go3DCoderSubBand<T>::go3DCoderSubBand ()
  : go3DCoder<T> () {
  bandIndex = 0;
  stages    = 1;
  hiPassCoder.setParameter(3);
  this->ID  = GO_ID_3DCODERSUBBAND;
}

template< class T >
go3DCoderSubBand<T>::~go3DCoderSubBand () {
}

template< class T >
void
go3DCoderSubBand<T>::init (void *) {
  loPassCoder.init(0);
  hiPassCoder.init(0);
}

#define GO_SUBBAND_ENCODESTAGE_BEGIN() {	\
  go3DSubBlock<T> __subBlocks[8];		\
  goSize_t __j;					

#define GO_SUBBAND_ENCODESTAGE_END() }

#define GO_SUBBAND_ENCODESTAGE_ONLYLO(__block) {					\
										\
    __subBlocks[0].setParent (__block);					\
    __subBlocks[0].setDiff (__subBlocks[0].getXDiff() << 1,			\
			     __subBlocks[0].getYDiff() << 1,			\
			     __subBlocks[0].getZDiff() << 1);		\
     __subBlocks[0].setSize (__block->getSizeX() >> 1,				\
			     __block->getSizeY() >> 1,				\
			     __block->getSizeZ() >> 1);			\
										 \
   loPassCoder.set3DBlock (&__subBlocks[0]);					\
   codedStream->resize(0);							\
   /*loPassCoder.setCodedStream (codedStream);*/					\
   loPassCoder.setStreamIndex(0);						\
   loPassCoder.encode();								\
   /*bandIndex[0] = 0;*/ 				                        \
 }

 #define GO_SUBBAND_ENCODESTAGE_ONLYHI(__block) {				\
										 \
   for (__j = 1; __j < 8; __j++) {						\
     __subBlocks[__j].setParent (__block);					\
     __subBlocks[__j].setDiff (__subBlocks[__j].getXDiff() << 1,			\
			       __subBlocks[__j].getYDiff() << 1,			\
			       __subBlocks[__j].getZDiff() << 1);		\
     __subBlocks[__j].setSize (__block->getSizeX() >> 1,				\
			       __block->getSizeY() >> 1,				\
			       __block->getSizeZ() >> 1);			\
   }										\
   __subBlocks[1].setPosition (1, 0, 0);						\
   __subBlocks[2].setPosition (1, 1, 0);						\
   __subBlocks[3].setPosition (0, 1, 0);						\
   __subBlocks[4].setPosition (0, 0, 1);						\
   __subBlocks[5].setPosition (1, 0, 1);						\
   __subBlocks[6].setPosition (1, 1, 1);						\
   __subBlocks[7].setPosition (0, 1, 1);						\
										 \
   (*coderParameter) += 0; /* Just to make 8 entries per stage */ \
   for (__j = 1; __j < 8; __j++) {						\
     bandIndex->resize (bandIndex->getSize() + 1);				\
     (*bandIndex)[bandIndex->getSize() - 1] = (goSize_t)codedStream->getSize();  \
     hiPassCoder.set3DBlock(&__subBlocks[__j]);					\
       /*hiPassCoder.setCodedStream(codedStream);*/				\
     hiPassCoder.setStreamIndex (0);						\
     hiPassCoder.encode();							\
     (*coderParameter) += hiPassCoder.getParameter();				\
   } \
 /* Write end of the last band */ \
   bandIndex->resize (bandIndex->getSize() + 1);				\
   (*bandIndex)[bandIndex->getSize() - 1] = (goSize_t)codedStream->getSize();	\
 }


 template< class T >
 void
 go3DCoderSubBand<T>::encode (goSize_t arrayIndex) {
   // bandIndex.resize(arrayIndex + 7);
   go3DBlock<T> tempBlock;
   tempBlock = *block;
   goSize_t s = stages - 1;
   goSize_t sx = tempBlock.getSizeX() >> s;
   goSize_t sy = tempBlock.getSizeY() >> s;
   goSize_t sz = tempBlock.getSizeZ() >> s;
   goPtrdiff_t dx = tempBlock.getXDiff() << s;
   goPtrdiff_t dy = tempBlock.getYDiff() << s;
   goPtrdiff_t dz = tempBlock.getZDiff() << s;
   goSize_t i;
   tempBlock.setSize (sx, sy, sz);
   tempBlock.setDiff (dx, dy, dz);
   GO_SUBBAND_ENCODESTAGE_BEGIN()
   GO_SUBBAND_ENCODESTAGE_ONLYLO((&tempBlock));
   for (i = 0; i < stages; i++) 
     {
       GO_SUBBAND_ENCODESTAGE_ONLYHI((&tempBlock));
       sx = sx << 1;
       sy = sy << 1;
       sz = sz << 1;
       dx = dx >> 1;
       dy = dy >> 1;
       dz = dz >> 1;
       tempBlock.setSize (sx, sy, sz);
       tempBlock.setDiff (dx, dy, dz);
     }
   GO_SUBBAND_ENCODESTAGE_END();
 //    T tmpT;
 //    for (i = 0; i < bandIndex.getSize(); i++) 
 //      {
 //        /* DANGER! */
 //        tmpT = (T)bandIndex[i];
 //        (*codedStream) += tmpT;
 //      }
 //    // cout << "bandIndex size after encoding: " << bandIndex.getSize() << endl;
 //    tmpT = (T)bandIndex.getSize();
 //    (*codedStream) += tmpT;
 }


 #define GO_SUBBAND_DECODESTAGE_BEGIN() {	\
   go3DSubBlock<T> __subBlocks[8];		\
   goSize_t __j;					

 #define GO_SUBBAND_DECODESTAGE_END() }

 /*!
  * Decodes the first stage of the encoded block into __block.
  */
 #define GO_SUBBAND_DECODESTAGE_ONLYLO(__block) {				 \
     __subBlocks[0].setParent (__block);					 \
     __subBlocks[0].setDiff (__subBlocks[0].getXDiff() << 1,			 \
			     __subBlocks[0].getYDiff() << 1,			 \
			     __subBlocks[0].getZDiff() << 1);		 \
     __subBlocks[0].setSize (__block->getSizeX() >> 1,				 \
			     __block->getSizeY() >> 1,				 \
			     __block->getSizeZ() >> 1);			 \
										  \
   loPassCoder.set3DBlock (&__subBlocks[0]);					 \
   loPassCoder.setStreamIndex (0);						\
    /*loPassCoder.setCodedStream (codedStream);*/	       			 \
   loPassCoder.decode();								 \
 }

 /*!
  * Decodes the stages 1 to __stage of the encoded block into __block. To decode the
  * first stage, use the macro GO_SUBBAND_DECODESTAGE() (see there).
  */
 #define GO_SUBBAND_DECODESTAGE_ONLYHI(__block,__stage) {       			 \
   for (__j = 1; __j < 8; __j++) {						 \
     __subBlocks[__j].setParent (__block);					 \
     __subBlocks[__j].setDiff (__subBlocks[__j].getXDiff() << 1,			 \
			       __subBlocks[__j].getYDiff() << 1,			 \
			       __subBlocks[__j].getZDiff() << 1);		 \
     __subBlocks[__j].setSize (__block->getSizeX() >> 1,				 \
			       __block->getSizeY() >> 1,				 \
			       __block->getSizeZ() >> 1);			 \
   }										 \
   __subBlocks[1].setPosition (1, 0, 0);					 \
   __subBlocks[2].setPosition (1, 1, 0);					 \
   __subBlocks[3].setPosition (0, 1, 0);					 \
   __subBlocks[4].setPosition (0, 0, 1);					 \
   __subBlocks[5].setPosition (1, 0, 1);					 \
   __subBlocks[6].setPosition (1, 1, 1);					 \
   __subBlocks[7].setPosition (0, 1, 1);					 \
   goSize_t __idx = arrayIndex + (8 * __stage);     	 \
   for (__j = 1; __j < 8; __j++) {						 \
     hiPassCoder.set3DBlock(&__subBlocks[__j]);					 \
      /*hiPassCoder.setCodedStream(codedStream, (*bandIndex)[__idx++]);*/	 \
      hiPassCoder.setStreamIndex((*bandIndex)[__idx++]);			 \
     hiPassCoder.setParameter ((*coderParameter)[__idx]);		\
     hiPassCoder.decode();							 \
   }										 \
 }

 template< class T >
 void
 go3DCoderSubBand<T>::decode (goSize_t decodeStages,
			      goSize_t arrayIndex) {
   go3DBlock<T> tempBlock;
   tempBlock = *block;
   goSize_t s = stages - 1;
   goSize_t sx = tempBlock.getSizeX() >> s;
   goSize_t sy = tempBlock.getSizeY() >> s;
   goSize_t sz = tempBlock.getSizeZ() >> s;
   goPtrdiff_t dx = tempBlock.getXDiff() << s;
   goPtrdiff_t dy = tempBlock.getYDiff() << s;
   goPtrdiff_t dz = tempBlock.getZDiff() << s;
   goSize_t i;
   tempBlock.setSize (sx, sy, sz);
   tempBlock.setDiff (dx, dy, dz);
   GO_SUBBAND_DECODESTAGE_BEGIN()  
   GO_SUBBAND_DECODESTAGE_ONLYLO((&tempBlock));
   for (i = 0; i < decodeStages; i++) 
    {
      GO_SUBBAND_DECODESTAGE_ONLYHI((&tempBlock),i);
      sx = sx << 1;
      sy = sy << 1;
      sz = sz << 1;
      dx = dx >> 1;
      dy = dy >> 1;
      dz = dz >> 1;
      tempBlock.setSize (sx, sy, sz);
      tempBlock.setDiff (dx, dy, dz);
    }
  GO_SUBBAND_DECODESTAGE_END()
}

template class go3DCoderSubBand< goInt32 >;











