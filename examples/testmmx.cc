/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <gommx.h>
#include <gommxmath.h>
#include <iostream>
#include <time.h>
#include <stdlib.h>

using namespace std;

int main()
{
  if (goCheckMMX())
    cout << "MMX enabled processor detected" << endl;
  else
    cout << "No MMX enabled processor detected" << endl;
  if (goCheck3DNOW())
    cout << "3DNOW! supporting processor detected" << endl;
  else
    cout << "No 3DNOW! detected" << endl;
  if (goCheck3DNOWDSP())
    cout << "3DNOW! DSP extensions detected" << endl;
  else
    cout << "No 3DNOW! DSP extensions" << endl;
  if (goCheck3DNOWEXT())
    cout << "3DNOW! MMX extensions detected" << endl;
  else
    cout << "No 3DNOW! MMX extensions" << endl;

  if (goCheck3DNOW())
      {
	  cout << "Performing 3DNow Mult2f" << endl;
	  float f1 = 3.0f;
	  float f2 = 2.0f;
	  float f3 = 5.0f;
	  float f4 = 4.0f;
	  float result[2];
	  result[0] = 0;
	  result[1] = 0;

	  int i;
	  clock_t t1,t2;
	  t1 = clock();
	  for (i = 0; i < 1000000; i++)
	      {
		  goASM3DNBegin();
		  goASM3DNMult2f (f1, f2, f3, f4, result);
		  goASM3DNEnd();
	      }
	  t2 = clock();
	  cout << "Time 3DNow: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
	  float _f1, _f2;
	  t1 = clock();
	  for (i = 0; i < 1000000; i++)
	      {
		  _f1 = f1 * f3;
		  _f2 = f2 * f4;
	      }
	  t2 = clock();
	  cout << "Time C: " << (t2 - t1) / (float)CLOCKS_PER_SEC << "s" << endl;
	  cout << "Result: " << result[0] << " and " << result[1] << endl;
      }
  exit (1);
}
