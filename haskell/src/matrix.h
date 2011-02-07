#ifdef __cplusplus 
extern "C"
{
#endif
#include <stdlib.h>

  int test ();


  typedef struct {
    void* object;
  } golib_matrix;

  golib_matrix* golib_matrix_new ();
  void golib_matrix_destroy (golib_matrix*);
  size_t golib_matrix_row_count (golib_matrix* m);
  size_t golib_matrix_col_count (golib_matrix* m);

#ifdef __cplusplus 
}
#endif

