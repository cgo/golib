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

int fromBool (bool b)
{
  switch (b)
  {
    case true: return 1; break;
    case false: return 0; break;
  }
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

static inline bool rangeCheck (golib_matrix* m, size_t i, size_t j)
{
  if (i < get_matrix_t(m)->getRows() && j < get_matrix_t(m)->getColumns())
  {
    return true;
  }
  else
  {
    return false;
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
  return static_cast<size_t> (get_matrix_t(m)->getRows ());
}

size_t golib_matrix_col_count (golib_matrix* m)
{
  return static_cast<size_t> (get_matrix_t(m)->getColumns ());
}

double golib_matrix_get_elem (golib_matrix* m, size_t row, size_t col)
{
  return (*static_cast<const matrix_t*>(get_matrix_t(m))) (row, col);
}

int golib_matrix_set_elem (golib_matrix* m, size_t row, size_t col, double elem)
{
  if (true != rangeCheck (m, row, col))
  {
    return fromBool (false);
  }
  get_matrix_t(m)->operator() (row, col) = elem;
  return fromBool (true);
}

void golib_matrix_fill (golib_matrix* m, double elem)
{
  get_matrix_t(m)->fill (elem);
}

int golib_matrix_transpose (golib_matrix* m)
{
  return fromBool (get_matrix_t(m)->transpose());
}

int golib_matrix_transpose_to (golib_matrix* source, golib_matrix* target)
{
  return fromBool (get_matrix_t(source)->getTranspose (*get_matrix_t(target)));
}

int golib_matrix_invert (golib_matrix* m)
{
  return fromBool (get_matrix_t(m)->invert ());
}

int golib_matrix_equals (golib_matrix* a, golib_matrix* b)
{
  if (*get_matrix_t(a) == *get_matrix_t(b))
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

void golib_matrix_copy (golib_matrix* source, golib_matrix* target)
{
  *get_matrix_t(target) = *get_matrix_t(source);
}


void golib_matrix_matrix_mult (double alpha, const golib_matrix* A, int transA,
			       const golib_matrix* B, int transB, double beta, golib_matrix* C)
{
  const matrix_t* AA = get_matrix_t (A);
  const matrix_t* BB = get_matrix_t (B);
  matrix_t* CC = get_matrix_t (C);
  goMath::matrixMult (alpha, *AA, transA == 1, *BB, transB == 1, beta, *CC);
}
