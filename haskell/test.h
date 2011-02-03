#ifdef __cplusplus 
extern "C"
{
#endif

  int test ();
  // #include <gomatrix.h>

  typedef struct {
    void* object;
  } golib_matrix;

  golib_matrix* golib_matrix_new ();
  void golib_matrix_destroy (golib_matrix*);

#ifdef __cplusplus 
}
#endif

