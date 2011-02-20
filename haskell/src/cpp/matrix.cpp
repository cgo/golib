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

matrix_t* get_matrix_t (golib_matrix* m)
{
  return static_cast<matrix_t*> (m->object);
}

const matrix_t* get_matrix_t (const golib_matrix* m)
{
  return static_cast<const matrix_t*> (m->object);
}

int golib_check_null_ptr (void* p)
{
  if (0 == p)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

golib_matrix* golib_matrix_new (size_t rows, size_t cols)
{
  matrix_t* m = 0;
  golib_matrix* ret = 0;

  m = new (std::nothrow) matrix_t (rows, cols);

  if (0 == m)
  {
    return 0;
  }

  ret = new (std::nothrow) golib_matrix;

  if (0 == ret)
    {
      delete m;
      return 0;
    }

  if (m->getColumns() != cols || m->getRows() != rows)
    {
      delete ret;
      delete m;
      return 0;
    }

  m->setIdentity ();

  ret->object = m;
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

void golib_matrix_matrix_mult (double alpha, const golib_matrix* A, int transA,
			       const golib_matrix* B, int transB, double beta, golib_matrix* C)
{
  const matrix_t* AA = get_matrix_t (A);
  const matrix_t* BB = get_matrix_t (B);
  matrix_t* CC = get_matrix_t (C);
  goMath::matrixMult (alpha, *AA, transA == 1, *BB, transB == 1, beta, *CC);
}
