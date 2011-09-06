/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <iostream>
#include <govector.h>
#include <gomatrix.h>
#include "vector.h"
#include "general.h"

// template class goMath::Vector<double>;

static inline bool rangeCheck (golib_vector* m, size_t i)
{
  if (i < get_vector_t(m)->getSize())
  {
    return true;
  }
  else
  {
    return false;
  }
}

golib_vector* golib_vector_new (size_t n)
{
  vector_t* m = 0;
  golib_vector* ret = 0;

  m = new (std::nothrow) vector_t (n);
  
  if (0 == m)
  {
    return 0;
  }

  ret = new (std::nothrow) golib_vector;

  if (0 == ret)
    {
      delete m;
      return 0;
    }

  if (m->getSize() != n)
    {
      delete ret;
      delete m;
      return 0;
    }

  m->fill (0);

  ret->object = m;
  return ret;
}

void golib_vector_destroy (golib_vector* m)
{
  delete static_cast<vector_t*>(m->object);
  delete m;
}

size_t golib_vector_size (golib_vector* m)
{
  return static_cast<size_t> (get_vector_t(m)->getSize ());
}

size_t golib_vector_stride (golib_vector* m)
{
  return static_cast<size_t> (get_vector_t(m)->getStride ());
}

double golib_vector_get_elem (golib_vector* m, size_t i)
{
  return (*static_cast<const vector_t*>(get_vector_t(m))) (i);
}

int golib_vector_set_elem (golib_vector* m, size_t i, double elem)
{
  if (true != rangeCheck (m, i))
  {
    return fromBool (false);
  }
  get_vector_t(m)->operator() (i) = elem;
  return fromBool (true);
}

void golib_vector_fill (golib_vector* m, double elem)
{
  get_vector_t(m)->fill (elem);
}


int golib_vector_equals (golib_vector* a, golib_vector* b)
{
  if (*get_vector_t(a) == *get_vector_t(b))
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

void golib_vector_copy (golib_vector* source, golib_vector* target)
{
  *get_vector_t(target) = *get_vector_t(source);
}

double golib_vector_dot (golib_vector* m1, golib_vector* m2)
{
  return *get_vector_t(m1) * *get_vector_t(m2);
}

void golib_vector_scalar_mult (golib_vector* m, double s)
{
  get_vector_t(m)->operator*= (s);
}

int golib_vector_add (double alpha, golib_vector* dst, golib_vector* v)
{
  return fromBool (goMath::vectorAdd (alpha, *get_vector_t(v), *get_vector_t(dst)));
}
