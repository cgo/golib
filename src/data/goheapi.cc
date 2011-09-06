/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#include <goheap.h>
#include <goheap.hpp>
#include <gotypes.h>

/* instantiation */
template class goHeap<goInt32, goInt32>;
template class goHeap<goFloat, goFloat>;
template class goHeap<goUInt32, goUInt32>;
template class goHeap<goUInt32, goFloat>;
