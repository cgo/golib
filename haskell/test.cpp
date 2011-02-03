#include <iostream>
#include <gomatrix.h>
#include "test.h"

int test ()
{
  std::cout << "Hello!" << "\n";
  return 10;
}

template class goMath::Matrix<double>;

golib_matrix* golib_matrix_new ()
{
  golib_matrix* ret = new golib_matrix;
  ret->object = new goMath::Matrix<double> (3,3);
}

void golib_matrix_destroy (golib_matrix* m)
{
  delete static_cast<goMath::Matrix<double>*>(m->object);
  delete m;
}
