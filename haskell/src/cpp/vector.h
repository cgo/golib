/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef _VECTOR_H_
#define _VECTOR_H_


#ifdef __cplusplus
extern "C" {
#endif
  typedef struct {
    void* object;
  } golib_vector;
#ifdef __cplusplus
}
#endif


#ifdef __cplusplus 

  typedef goMath::Vector<double> vector_t;
  
  static inline vector_t* get_vector_t (golib_vector* m)
  {
    return static_cast<vector_t*> (m->object);
  }

  static inline const vector_t* get_const_vector_t (const golib_vector* m)
  {
    return static_cast<const vector_t*> (m->object);
  }

extern "C"
{
#endif
#include <stdlib.h>

  golib_vector* golib_vector_new (size_t n);
  void          golib_vector_destroy (golib_vector*);
  size_t        golib_vector_size (golib_vector* m);
  double        golib_vector_get_elem (golib_vector* m, size_t n);
  int           golib_vector_set_elem (golib_vector* m, size_t n, double elem);
  void          golib_vector_fill (golib_vector* m, double elem);

  //  void          golib_matrix_matrix_mult (double alpha, const golib_matrix* A, int transA,
  //					  const golib_matrix* B, int transB, double beta, golib_matrix* C);

  void          golib_vector_copy (golib_vector* source, golib_vector* target);
  int           golib_vector_equals (golib_vector* a, golib_vector* b);
  void          golib_vector_scalar_mult (golib_vector* m, double s);
  double        golib_vector_dot (golib_vector* m1, golib_vector* m2);
  int           golib_vector_add (double, golib_vector* dst, golib_vector* v);

#ifdef __cplusplus 
}
#endif

#endif /* _MATRIX_H_ */
