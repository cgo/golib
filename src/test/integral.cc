/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <math/gointegral.h>
#include <stdio.h>
#include <defs/gotypes.h>
#include <math.h>

int main () {
  double d = 0;
  double distance = 0.1;
  goIntegral<double>	integral;
  goArray<double>	a;

  for (d = 0; d < M_PI; d+=distance) {
    a+=(double)cos(d);
  }
  integral.setFunction (a, distance);
  d = integral.eval ();
  cout << "evaluated integral to " << d << "\n";

  exit(0);
}
