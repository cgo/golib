#include <gofilterfir.h>

#include <time.h>


void* filterThread (void* p) {
  goFilterFIRInfo* _p = (goFilterFIRInfo*)p;
  switch (_p->type) {
  case GO_INT32: ((goFilterFIR<goInt32>*)(_p->filterPtr))->filterThread(); break;
  case GO_DOUBLE: ((goFilterFIR<goDouble>*)(_p->filterPtr))->filterThread(); break;
  case GO_FLOAT: ((goFilterFIR<goFloat>*)(_p->filterPtr))->filterThread(); break;
  default: break;
  }
  return 0;
}


template <class T>
goFilterFIR<T>::goFilterFIR (goSize_t maskWidth, goSize_t maskHeight) 
  : goFilter<T> () {
  mask.resize (maskWidth * maskHeight);
  sizeX = maskWidth;
  sizeY = maskHeight;

  centerX = sizeX >> 1;
  centerY = sizeY >> 1;
}

template <class T>
goFilterFIR<T>::~goFilterFIR() {
  mask.resize(0);
}

template <class T>
void
goFilterFIR<T>::setMask (goDouble *m) {
  goIndex_t i;
  for (i = 0; i < mask.getSize(); i++) {
    mask[i] = m[i];
  }
}


template <class T>
void
goFilterFIR<T>::setCenter (goIndex_t x, goIndex_t y) {
  centerX = x;
  centerY = y;
}

void goFilterFIR<goInt32>::setFilterInfo (goFilterFIRInfo &info) {
  info.type = GO_INT32;
  info.filterPtr = (void*)this;
}
void goFilterFIR<goDouble>::setFilterInfo (goFilterFIRInfo &info) {
  info.type = GO_DOUBLE;
  info.filterPtr = (void*)this;
}
void goFilterFIR<goFloat>::setFilterInfo (goFilterFIRInfo &info) {
  info.type = GO_FLOAT;
  info.filterPtr = (void*)this;
}



template <class T>
void
goFilterFIR<T>::filter (goSignal2D<T>& s, goSignal2D<T>& target, goInt32 step) {
  s_ptr = &s;
  target_ptr = &target;

  /* 
   * Initialize the goThread object and start as many threads as there are processors
   */
  goFilterFIRInfo info;
  setFilterInfo (info);

  cout << "Number of processors: " << goThread::howManyProcessors() << endl;

  threads.resetInt();
  // threads.create (::filterThread, (void*)&info, goThread::howManyProcessors() - 1);
  threads.create (::filterThread, (void*)&info, 4);
  
  cout << "Number of threads created: " << threads.getNumOfThreads() << endl;
  
  s_ptr = &s;
  target_ptr = &target;
  filterThread (step);
  threads.join();
}

template <class T>
void
goFilterFIR<T>::filterThread (goInt32 step) {
  register goIndex_t x = 0;
  register goIndex_t y = 0;
  register goDouble sum = 0;
  goIndex_t width  = (goIndex_t)s_ptr->getSizeX();
  goIndex_t height = (goIndex_t)s_ptr->getSizeY();

  int threadID = threads.makeInt();
  int numOfThreads = threads.getNumOfThreads ();


  width = (goIndex_t)(width / (float)(numOfThreads + 1));
  goIndex_t myStartX = width * threadID;
  goIndex_t myStartY = 0;
  goIndex_t myEndX;
  
  if (threadID == (numOfThreads - 1)) {
    myEndX = (goIndex_t)s_ptr->getSizeX();
  } else {
    myEndX = myStartX + width;
  }



  // Use pointers when possible instead getPtr functions (40-50% faster)
  T*		 linePtr;
  T*		 linePtrSave;
  T*		 linePtrStart;
  T*		 linePtrStartSave;
  T*		 targetLinePtrStart;
  T*		 targetLinePtr;
  int		 targetLinePtrOffsetX = 1;
  int		 targetLinePtrOffsetY = (target_ptr->getSizeX() + 2 * target_ptr->getBorderX()) * targetLinePtrOffsetX;
  register int		 linePtrOffsetX = 1;
  register int		 linePtrOffsetY = (s_ptr->getSizeX() + 2 * s_ptr->getBorderX()) * linePtrOffsetX;
  goIndex_t fx, fy;

  register goSize_t xd, yd;

  goDouble	*maskPtr;
  goDouble	*maskPtrStart = mask.getPtr();
  
  // fx = -(sizeX >> 1) + myStartX;  
  // fy = -(sizeY >> 1) + myStartY;
  fx = -centerX + myStartX;
  fy = -centerY + myStartY;
  linePtrStartSave = linePtrStart = s_ptr->getPtr (fx, fy);
  targetLinePtrStart = target_ptr->getPtr (myStartX, myStartY);
  // clock_t t1 = clock();
  for (y = myStartY; y < (myStartY + height); y++) {
    targetLinePtr = targetLinePtrStart;
    // filter line
    for (x = myStartX; x < myEndX; x++) {
      linePtr = linePtrStart;
      maskPtr = maskPtrStart;
      sum = 0;
      // filter point at x,y
      for (yd = 0; yd < sizeY; yd++) {
	linePtrSave = linePtr;
	for (xd = 0; xd < sizeX; xd++) {
	  /// OPTIMIZE THE CAST AWAY !
	  sum += (goDouble)(*linePtr * (*maskPtr));
	  linePtr = linePtr + linePtrOffsetX;
	  maskPtr++;
	}
	linePtr = linePtrSave + linePtrOffsetY;
      }
      linePtrStart += linePtrOffsetX;
      *targetLinePtr = (T)sum;
      targetLinePtr = targetLinePtr + targetLinePtrOffsetX;
    }
    linePtrStart = linePtrStartSave = linePtrStartSave + linePtrOffsetY;
    targetLinePtrStart = targetLinePtrStart + targetLinePtrOffsetY;
  }
  // clock_t t2 = clock();
  // cout << "FIR filter: time in loop was " << (t2-t1) / (float)CLOCKS_PER_SEC << endl;
}


template class goFilterFIR<goDouble>;
template class goFilterFIR<goFloat>;
template class goFilterFIR<goInt32>;

/*
template class goFilterFIRThread<goDouble>;
template class goFilterFIRThread<goInt32>;
*/










