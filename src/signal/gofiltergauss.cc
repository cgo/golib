#include <gofiltergauss.h>

#include <time.h>

template <class T>
goFilterGauss<T>::goFilterGauss (goSize_t maskLength)
  : goFilter<T>() {
  length = maskLength;
  mask.resize (length);
  switch (length) {
  case 3:
    mask[0] = 0.25; mask[1] = 0.5; mask[2] = 0.25;
    break;
  case 5: 
    mask[0] = 0.0625; mask[1] = 0.25; mask[2] = 0.375; mask[3] = 0.25; mask[4] = 0.0625;
    break;
  case 7:
    mask[0] = 0.015625; mask[1] = 0.09375; mask[2] = 0.234375; mask[3] = 0.3125;
    mask[4] = 0.234375; mask[5] = 0.09375; mask[6] = 0.015625;
    break;
  default:
    cout << "Setting default Gauss filter length to 3 " << endl;
    length = 3;
    mask.resize(3);
    mask[0] = 0.25; mask[1] = 0.5; mask[2] = 0.25;    
    break;
  }
}

template <class T>
goFilterGauss<T>::~goFilterGauss () {
  mask.resize (0);
}

template <class T>
void
goFilterGauss<T>::filter (goSignal2D<T>& src, goSignal2D<T>& target) {
  register goSize_t x, y;

  register T* srcLinePtr;
  register T* srcLinePtrSave;
  register T* srcLinePtrStart;
  register T* targetLinePtr;
  register T* targetLinePtrStart;
  register goPtrOffset_t srcOffsetX = src.getOffsetX();
  register goPtrOffset_t srcOffsetY = src.getOffsetY();
  register goPtrOffset_t targetOffsetX = target.getOffsetX();
  register goPtrOffset_t targetOffsetY = target.getOffsetY();
  
  register goSize_t width  = src.getSizeX();
  register goSize_t height = src.getSizeY();

  srcLinePtrStart    = srcLinePtr    = src.getPtr (-(length >> 1),0);
  targetLinePtrStart = targetLinePtr = target.getPtr (0,0);
  
  goDouble *maskPtr, *maskPtrStart;
  maskPtr = maskPtrStart = mask.getPtr();
  register goSize_t i;
  register goDouble sum;
  /// Horizontal
  clock_t t1 = clock ();
  for (y = 0; y < height; y++) {
    for (x = 0; x < width; x++) {
      sum = 0.0;
      srcLinePtrSave = srcLinePtr;
      for (i = 0; i < length; i++) {
	sum += *maskPtr * *srcLinePtr;
	maskPtr++;
	srcLinePtr += srcOffsetX;
      }
      maskPtr = maskPtrStart;
      *targetLinePtr = (T)sum;
      srcLinePtr = srcLinePtrSave + srcOffsetX;
      targetLinePtr += targetOffsetX;
    }
    srcLinePtr = srcLinePtrStart = srcLinePtrStart + srcOffsetY;
    targetLinePtr = targetLinePtrStart = targetLinePtrStart + targetOffsetY;
  }

  /// Vertical
  srcOffsetX = target.getOffsetY();
  srcOffsetY = target.getOffsetX();
  targetOffsetX = src.getOffsetY();
  targetOffsetY = src.getOffsetX();
  srcLinePtrStart    = srcLinePtr    = target.getPtr (0, -(length >> 1));
  targetLinePtrStart = targetLinePtr = src.getPtr (0,0);

  for (y = 0; y < width; y++) {
    for (x = 0; x < height; x++) {
      sum = 0.0;
      srcLinePtrSave = srcLinePtr;
      for (i = 0; i < length; i++) {
	sum += *maskPtr * *srcLinePtr;
	maskPtr++;
	srcLinePtr += srcOffsetX;
      }
      maskPtr = maskPtrStart;
      *targetLinePtr = (T)sum;
      srcLinePtr = srcLinePtrSave + srcOffsetX;
      targetLinePtr += targetOffsetX;
    }
    srcLinePtr = srcLinePtrStart = srcLinePtrStart + srcOffsetY;
    targetLinePtr = targetLinePtrStart = targetLinePtrStart + targetOffsetY;
  }
  clock_t t2 = clock ();
  cout << "Gauss filter: time in loop was " << (t2-t1) / (float)CLOCKS_PER_SEC << endl;

}

template class goFilterGauss<goDouble>;
template class goFilterGauss<goInt32>;





