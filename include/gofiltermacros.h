/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


/*!
 * \file Macro definitions for some filters. This is intended to be a fast
 * filter implementation for my diploma thesis.
 * \author Christian Gosch
 */

#ifndef GOFILTERMACROS_H
#define GOFILTERMACROS_H

/*!
 * Accumulates the element-wise multiplication of the arrays of length 
 * <code>length</code> stored in
 * start_pointer and filter_pointer in <code>result</code>.
 * start_pointer and filter_pointer <strong>are modified</strong> 
 * and point to the elements
 * xxxx_pointer[length] after the macro is done.
 */
#define GO_FILTER_MULT(result, start_pointer, filter_pointer, length) {	\
  goIndex_t __i;							\
  result = 0;								\
  for (__i = 0; __i < length; __i++)					\
    {									\
      result += (*start_pointer * *filter_pointer);			\
      start_pointer++; filter_pointer++;				\
    }									\
}

#define GO_FILTER_MULT4(result, start_pointer, filter_pointer) {	\
  result = *start_pointer * *filter_pointer + 				\
    start_pointer[1] * filter_pointer[1] +				\
    start_pointer[2] * filter_pointer[2] +				\
    start_pointer[3] * filter_pointer[3];				\
}

#endif
