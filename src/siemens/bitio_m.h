/**************************************************************************
 *
 * bitio_m.h -- Macros for bitio
 * Copyright (C) 1994  Neil Sharman and Alistair Moffat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: bitio_m.h,v 1.1 2001/07/02 18:58:23 christian Exp $
 *
 **************************************************************************/


#ifndef H_BITIO_M
#define H_BITIO_M

#include <stdio.h>

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

#ifndef BIO_ENCODE_PROLOGUE
#define BIO_ENCODE_PROLOGUE
#endif

#ifndef BIO_DECODE_PROLOGUE
#define BIO_DECODE_PROLOGUE
#endif

#ifndef BIO_ENCODE_EPILOGUE
#define BIO_ENCODE_EPILOGUE
#endif

#ifndef BIO_DECODE_EPILOGUE
#define BIO_DECODE_EPILOGUE
#endif

#ifndef DECODE_ADD
#define DECODE_ADD(b) (b) += (b) + DECODE_BIT
#endif

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

#define POSITIVE(f, x)							\
  if ((x)<=0) 								\
    fprintf(stderr,"Error: Cannot "#f" encode %lu\n",(unsigned long)x),exit(1);


#define CEILLOG_2(x,v)							\
do {									\
  register int _B_x  = (x) - 1;						\
  (v) = 0;								\
  for (; _B_x ; _B_x>>=1, (v)++);					\
} while(0)


/****************************************************************************/


#define UNARY_ENCODE(x)							\
do {									\
  register long _B_x = (x);					\
  BIO_ENCODE_PROLOGUE;							\
  POSITIVE(unary, _B_x);						\
  while(--_B_x) ENCODE_BIT(0);						\
  ENCODE_BIT(1);							\
  BIO_ENCODE_EPILOGUE;							\
} while(0)

#define UNARY_DECODE(x)							\
do {									\
  BIO_DECODE_PROLOGUE;							\
  (x) = 1;								\
  while (!DECODE_BIT) (x)++;						\
  BIO_DECODE_EPILOGUE;							\
} while(0)

#define UNARY_LENGTH(x, count)						\
do {									\
  POSITIVE(unary, x);							\
  (count) = (x);							\
} while(0)


/****************************************************************************/


#define BINARY_ENCODE(x, b)						\
do {									\
  register long _B_x = (x);					\
  register long _B_b = (b);					\
  register int _B_nbits, _B_logofb, _B_thresh;				\
  BIO_ENCODE_PROLOGUE;							\
  POSITIVE(binary, _B_x);						\
  CEILLOG_2(_B_b, _B_logofb);						\
  _B_thresh = (1<<_B_logofb) - _B_b;					\
  if (--_B_x < _B_thresh)						\
    _B_nbits = _B_logofb-1;						\
  else									\
    {									\
      _B_nbits = _B_logofb;						\
      _B_x += _B_thresh;						\
    }									\
  while (--_B_nbits>=0)							\
    ENCODE_BIT((_B_x>>_B_nbits) & 0x1);					\
  BIO_ENCODE_EPILOGUE;							\
} while(0)

#define BINARY_DECODE(x, b)						\
do {									\
  register long _B_x = 0;					\
  register long _B_b = (b);					\
  register int _B_i, _B_logofb, _B_thresh;				\
  BIO_DECODE_PROLOGUE;							\
  if (_B_b != 1)							\
    {									\
      CEILLOG_2(_B_b, _B_logofb);					\
      _B_thresh = (1<<_B_logofb) - _B_b;				\
      _B_logofb--;							\
      for (_B_i=0; _B_i < _B_logofb; _B_i++)			 	\
        DECODE_ADD(_B_x);						\
      if (_B_x >= _B_thresh)						\
        {								\
          DECODE_ADD(_B_x);						\
          _B_x -= _B_thresh;						\
	}								\
      (x) = _B_x+1;							\
    }									\
  else									\
    (x) = 1;								\
  BIO_DECODE_EPILOGUE;							\
} while(0)


#define BINARY_LENGTH(x, b, count)					\
do {									\
  register unsigned long _B_x = (x);					\
  register unsigned long _B_b = (b);					\
  register int _B_nbits, _B_logofb, _B_thresh;				\
  POSITIVE(binary, _B_x);						\
  CEILLOG_2(_B_b, _B_logofb);						\
  _B_thresh = (1<<_B_logofb) - _B_b;					\
  if (--_B_x < _B_thresh)						\
    _B_nbits = _B_logofb-1;						\
  else									\
    _B_nbits = _B_logofb;						\
  (count) = _B_nbits;							\
} while(0)

/****************************************************************************/

#endif
