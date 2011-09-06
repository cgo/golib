/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef _MATRIX_H_
#define _MATRIX_H_

#ifdef __cplusplus 
extern "C"
{
#endif
#include <stdlib.h>
#include "vector.h"

  int test ();


  typedef struct {
    void* object;
  } golib_matrix;

  golib_matrix* golib_matrix_new (size_t rows, size_t cols);
  void          golib_matrix_destroy (golib_matrix*);
  size_t        golib_matrix_row_count (golib_matrix* m);
  size_t        golib_matrix_col_count (golib_matrix* m);
  double        golib_matrix_get_elem (golib_matrix* m, size_t row, size_t col);
  int           golib_matrix_set_elem (golib_matrix* m, size_t row, size_t col, double elem);
  void          golib_matrix_fill (golib_matrix* m, double elem);
  int           golib_matrix_transpose (golib_matrix* m);
  int           golib_matrix_transpose_to (golib_matrix* source, golib_matrix* target);
  int           golib_matrix_invert (golib_matrix* m);

  void          golib_matrix_matrix_mult (double alpha, const golib_matrix* A, int transA,
					  const golib_matrix* B, int transB, double beta, golib_matrix* C);

  void          golib_matrix_copy (golib_matrix* source, golib_matrix* target);
  int           golib_matrix_equals (golib_matrix* a, golib_matrix* b);

  int           golib_matrix_vector_mult (double alpha, const golib_matrix* m, int transA, 
                                          const golib_vector* v,
                                          double beta, golib_vector* ret);


  void          golib_matrix_scalar_mult (golib_matrix* m, double s);
  int           golib_matrix_add (golib_matrix* m, const golib_matrix* m2);
  int           golib_matrix_sub (golib_matrix* m, const golib_matrix* m2);

#ifdef __cplusplus 
}
#endif

#endif /* _MATRIX_H_ */
