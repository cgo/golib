/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


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

