#include <iostream.h>
#include <signal/gosignaltransform.h>
#include <signal/gosignal.h>

int main () {
  
  goSignalTransform<double>	t;

  t.setOffset (6);
  t.resize (13);
  t[0] = 0;
  t[1] = 0;
  t[2] = 0;
  t[3] = 0;
  t[4] = 1;
  t[5] = 1;
  t[6] = 1;
  t[7] = 1;
  t[8] = 1;
  t[9] = 0;
  t[10] = 0;
  t[11] = 0;
  t[12] = 0;
  t.zeroPadding (100);
  cout << "t = " << t << "\n";

  t.dft();
  cout << "T = " << *((goSignal<goComplex<goDouble> >*)t.getTransform()) << "\n";
  
  exit(0);
}
