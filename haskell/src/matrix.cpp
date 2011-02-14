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

golib_matrix* golib_matrix_new (size_t rows, size_t cols)
{
  matrix_t*	m   = new matrix_t (rows, cols);
  if (0 == m)
  {
    return 0;
  }
  m->setIdentity ();
  golib_matrix* ret = new golib_matrix;
  if (0 == ret)
  {
    delete m;
    return 0;
  }
  ret->object	    = m;
  return ret;
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

double golib_matrix_get_elem (golib_matrix* m, size_t row, size_t col)
{
  matrix_t* mat = static_cast<matrix_t*>(m->object);
  return (*mat)(row,col);
}
