#include <defs/gotypes.h>
#include <math/go44matrix.h>
#include <math/go4vector.h>
#include <iostream.h>

int main (void) {
  go44Matrix<goFloat> m1;
  goFloat f[16] = { 1,2,3,0,
		    5,6,7,0,
		    9,8,7,0,
		    0,0,0,1 };
  go4Vector<goFloat>  v1 (4,3,2,1) , v2;
  
  
  cout << m1;
  m1.fill (f);
  cout << m1;
  m1 += m1;
  cout << m1;
  
  cout << v1;
  v2 = v1;
  v1 *= m1;
  
  cout << v1;
  
  cout << v2 * v1;

  
}
