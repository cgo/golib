/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */



#include <gotypes.h>
#include <gosignal3d.h>
#include <gofiltermacros.h>
#include <gosignalmacros.h>

#include <time.h>
#include <iostream.h>

#define SIGNAL_TYPE goFloat
#define MASK_TYPE   goFloat

int main (int argc, char* argv[])
{
  if (argc < 4)
    {
      cout << "libGo 3D filter example." << endl;
      cout << "Usage:" << endl;
      cout << argv[0] << " <x size> <y size> <z size>" << endl << endl;
      exit(1);
    }
  goSignal3D<SIGNAL_TYPE>	signal;

  clock_t t1,t2;
  t1 = clock();
  signal.make (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
  t2 = clock();
  cout << "Signal memory allocated in " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
  cout << "t1 = " << t1 << endl << "t2 = " << t2 << endl;

  /*
   * Filter macro test
   */
  cout << "Filter macro test" << endl;
  cout << "-----------------" << endl;
  MASK_TYPE mask[4] = {1,2,1,2};
  goFloat r = 0;
  MASK_TYPE *maskPtr = &mask[0];
  SIGNAL_TYPE *signalPtr = signal.getPtr();
  *(signalPtr++) = 1;
  *(signalPtr++) = 2;
  *(signalPtr++) = 3;
  *(signalPtr++) = 4;
  signalPtr = signal.getPtr();
  SIGNAL_TYPE *tempF = signalPtr;
  MASK_TYPE *tempF2 = mask;
  t1 = clock();
  int i;
  int sz = (atoi(argv[1]) * atoi(argv[2]) * atoi(argv[3]));
  for (i = 0; i < sz; i++)
    {
      //signalPtr = tempF;
      //maskPtr = tempF2;
      GO_FILTER_MULT4(r, signalPtr, maskPtr);
    }
  t2 = clock();  
  cout << "Time for " << i << " filter operations on the same memory: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
  
  cout << "r = " << r << endl;

  /*
   * Filter macro test with signal macros
   */
  cout << endl << "Filter macro test with signal macros" << endl;
  cout << "------------------------------------" << endl;
  goSignal3D<SIGNAL_TYPE> signal2;
  
  signal2.make (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
  t1 = clock();
  GO_SIGNAL3D_EACHELEMENT_2( GO_FILTER_MULT4((*__ptr_target), __ptr, maskPtr), \
			     signal, \
			     signal2, \
			     SIGNAL_TYPE, SIGNAL_TYPE);
  t2 = clock();
  cout << "Time to filter the signal in x direction: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
  signal2.rotateAxes();
  signal.rotateAxes();
  t1 = clock();
  GO_SIGNAL3D_EACHELEMENT_2( GO_FILTER_MULT4((*__ptr_target), __ptr, maskPtr), \
			     signal2, \
			     signal, \
			     SIGNAL_TYPE, SIGNAL_TYPE);
  t2 = clock();
  cout << "Time to filter the signal in z direction: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
  signal2.rotateAxes();
  signal.rotateAxes();
  t1 = clock();
  GO_SIGNAL3D_EACHELEMENT_2( GO_FILTER_MULT4((*__ptr_target), __ptr, maskPtr), \
			     signal, \
			     signal2, \
			     SIGNAL_TYPE, SIGNAL_TYPE);
  t2 = clock();
  cout << "Time to filter the signal in y direction: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
  signal2.rotateAxes();
  signal.rotateAxes();
  

  signal.destroy();
  signal2.destroy();
  exit(1);
}

