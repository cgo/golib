/**************************************************************************
 *
 * bitio_m_stdio.h -- Macros for bitio to a file
 * Copyright (C) 1994  Neil Sharman
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
 * $Id: bitio_m_stdio.h,v 1.1 2001/07/02 18:58:23 christian Exp $
 *
 **************************************************************************
 *
 *  This file contains macros for doing bitwise input and output on a FILE*.
 *  With these routines you cannot mix reads and writes on the FILE,
 *  or multiple writes, at the same time and guarantee them to work, also you 
 *  cannot seek to a point and do a write. The decode routine can detect when
 *  you run off the end of the file and will produce an approate error message.
 *
 *
 **************************************************************************/

#ifndef H_BITIO_M_STDIO
#define H_BITIO_M_STDIO

#include <stdio.h>
#include <iostream.h>

typedef struct stdio_bitio_state
  {
    FILE *File;
    unsigned char Buff;
    unsigned char Btg;
  }
stdio_bitio_state;



#ifndef DECODE_ERROR
#define DECODE_ERROR (fprintf(stderr,"Unexpected EOF in \"%s\" on line %d at voxel (%d,%d,%d)\n",__FILE__, __LINE__,x,y,z), exit(1))
#endif



#define ENCODE_START(f)							\
  {									\
    goArray<T> *__outarray = f;						\
    register T __buff = 0;					\
    register unsigned char __btg = sizeof(T)*8;			


#define ENCODE_BIT(b)							\
  do {									\
    __btg--;								\
    if (b) __buff |= (1 << __btg);					\
    if (!__btg)								\
      {									\
	/* putc(__buff, __outfile); */					\
        (*__outarray) += __buff;						\
	__buff = 0;							\
	__btg = sizeof(T)*8;					\
      }									\
  } while(0)


#define ENCODE_DONE							\
    if (__btg != sizeof(T)*8)					\
      /* putc(__buff,__outfile); */    					\
      (*__outarray) += __buff;						\
  }


#define DECODE_START(f,idx)							\
  {									\
    goArray<T> *__inarray = f;						\
    register T __buff = 0;						\
    register unsigned char __btg = 0;					\
    register int __inarrayindex = idx;


#define DECODE_BIT (__btg ? (__buff >> --__btg) & 1 :			\
   (__buff = (*__inarray)[__inarrayindex++], ( (__inarrayindex == __inarray->getSize() + 1) ?				\
    (DECODE_ERROR, 0) :							\
    (__btg = sizeof(T)*8, (__buff >> --__btg) & 1))))

#define DECODE_DONE	;						\
  }

#endif



