#include <math/gocomplex.h>
#include <iostream.h>

int main (void) {
  goComplex<double> d1,d2;
  goComplex<double> d3(3,2);

  printf("test !\n");

  cout << "d3 = " << d3 << "\n";

  d1.re() = 2;
  d1.im() = 4.3;
  cout << "d1 = " << d1 << "\n";
  
  cout << "d1 * d3 = " << d1*d3 << "\n";
  cout << "d1 / d3 = " << d1/d3 << "\n";
  cout << "d1 + d3 = " << d1+d3 << "\n";
  cout << "d1 - d3 = " << d1-d3 << "\n";

  d2.re() = 1;
  d2.im() = 0;
  cout << "d2 = " << d2 << "\n";
  cout << "arg(d2) = " << d2.arg() << "\n";

  d2.re() = 1;
  d2.im() = 1;
  cout << "d2 = " << d2 << "\n";
  cout << "arg(d2) = " << d2.arg() << "\n";

  d2.re() = 1;
  d2.im() = -1;
  cout << "d2 = " << d2 << "\n";
  cout << "arg(d2) = " << d2.arg() << "\n";

  d2.re() = -1;
  d2.im() = 1;
  cout << "d2 = " << d2 << "\n";
  cout << "arg(d2) = " << d2.arg() << "\n";

  d2.re() = -5;
  d2.im() = -1;
  cout << "d2 = " << d2 << "\n";
  cout << "arg(d2) = " << d2.arg() << "\n";

  exit(0);
}








