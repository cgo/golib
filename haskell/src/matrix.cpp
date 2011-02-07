#include <iostream>
#include <gomatrix.h>
#include "matrix.h"

int test ()
{
  std::cout << "Hello!" << "\n";
  return 10;
}

template class goMath::Matrix<double>;

typedef goMath::Matrix<double> matrix_t;

golib_matrix* golib_matrix_new ()
{
  golib_matrix* ret = new golib_matrix;
  ret->object = new matrix_t (3,3);
}

void golib_matrix_destroy (golib_matrix* m)
{
  std::cout << "Deleting a matrix...";
  delete static_cast<matrix_t*>(m->object);
  delete m;
  std::cout << "done deleting.\n";
}

size_t golib_matrix_row_count (golib_matrix* m)
{
  return static_cast<size_t> (static_cast<matrix_t*>(m->object)->getRows ());
}

size_t golib_matrix_col_count (golib_matrix* m)
{
  return static_cast<size_t> (static_cast<matrix_t*>(m->object)->getColumns ());
}
