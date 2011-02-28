#ifndef _VECTOR_H_
#define _VECTOR_H_

#ifdef __cplusplus 
extern "C"
{
#endif
#include <stdlib.h>


  typedef struct {
    void* object;
  } golib_vector;


  typedef goMath::Vector<double> vector_t;
  
  static inline vector_t* get_vector_t (golib_vector* m)
  {
    return static_cast<vector_t*> (m->object);
  }

  static inline const vector_t* get_const_vector_t (const golib_vector* m)
  {
    return static_cast<const vector_t*> (m->object);
  }


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

#ifdef __cplusplus 
}
#endif

#endif /* _MATRIX_H_ */
