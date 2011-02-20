#ifdef __cplusplus 
extern "C"
{
#endif
#include <stdlib.h>

  int test ();


  typedef struct {
    void* object;
  } golib_matrix;

  int           golib_check_null_ptr (void* p);
  golib_matrix* golib_matrix_new (size_t rows, size_t cols);
  void		golib_matrix_destroy (golib_matrix*);
  size_t	golib_matrix_row_count (golib_matrix* m);
  size_t	golib_matrix_col_count (golib_matrix* m);
  double	golib_matrix_get_elem (golib_matrix* m, size_t row, size_t col);

  void          golib_matrix_matrix_mult (double alpha, const golib_matrix* A, int transA,
					  const golib_matrix* B, int transB, double beta, golib_matrix* C);

  // void       golib_matrix_scalar_mult (double s, golib_matrix* m);

#ifdef __cplusplus 
}
#endif

