/**************************************************************************
 *
 * felics.c -- The main guts of the felics algorithm
 * Copyright (C) 1994  Neil Sharman and Kerry Guise
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
 * $Id: felics.cxx,v 1.1 2001/07/02 18:58:23 christian Exp $
 *
 **************************************************************************/

#include <bitio_m.h>
#include <bitio_m_stdio.h>
#include <gotypes.h>
#include <go3dblock.h>
#include <goarray.h>
#include <gohashtable.h>

#include <iostream.h>

#include <stdlib.h>
#include <string.h>

#define INTSIZE sizeof(goInt32) * 8
#define BOOLEAN short
#define TRUE 1
#define FALSE 0

#define IN_RANGE 0
#define OUT_OF_RANGE 1
#define BELOW_RANGE 0
#define ABOVE_RANGE 1
#define NEGATIVE_BIT 1
#define POSITIVE_BIT 0

template< class T >
void 
goFELICSencode (go3DBlock<T> *block, 
		T maxval, int maxk,
		goArray<T> *array)
{
  goSize_t x, y, z;
  long P, L = 0, H = 0, N1, N2, delta;
  goHashTable<goUInt32,void*> cumul_param;
  goUInt32* cumul_param_ptr = 0;
  // unsigned long cumul_param[256][INTSIZE];
  unsigned max_cumul;

  // printf("Setting cumul_param to zero...\n");
  // memset (cumul_param, '0', sizeof (cumul_param));

  if (maxk)
    max_cumul = maxk;
  else
    CEILLOG_2 (maxval, max_cumul);

  ENCODE_START (array)

    
    T *line;
  goPtrdiff_t dx = block->getXDiff();
  goPtrdiff_t dy = block->getYDiff();
  for (z = 0; z < block->getSizeZ(); z++) {
    for (y = 0; y < block->getSizeY(); y++) {
      line = block->getPtr (0,y,z);
      for (x = 0; x < block->getSizeX(); x++)
	{
	  if (y > 0)
	    {
	      if (x > 0)
		{			/* ******** */
		  N1 = *(line + (x * dx) - dy);	/* ****1*** */
		  N2 = *(line + ((x - 1) * dx));	/* ***2P*** */
		}			/* ******** */
	      else
		{			/* |******* */
		  N1 = *(line + (x * dx) - dy);	/* |12***** */
		  N2 = *(line + ((x + 1) * dx) - dy );	/* |P***** */
		}			/* |******* */
//  		{			/* ******** */
//  		  N1 = *(line + (x * dx));	/* ****1*** */
//  		  N2 = *(line + (x - 1)*dx);	/* ***2P*** */
//  		}			/* ******** */
//  	      else
//  		{			/* |******* */
//  		  N1 = *(line + (x * dx));	/* |12***** */
//  		  N2 = *(line + ((x + 1) * dx));	/* |P***** */
//		}			/* |******* */
	    }
	  else
	    {
	      if (x > 1)
		{
		  N1 = *(line + ((x - 1) * dx));	/* -------- */
		  N2 = *(line + ((x - 2) * dx));	/* **12P*** */
		}			/* ******** */
	      else if (x > 0)
		{
		  N1 = *(line + ((x - 1) * dx));	/* +------- */
		  N2 = N1;	/* |3P***** */
		}			/* |******* */
	      else
		{
		  // line[x * dx] = get (fp_in);	/* +------- */
		  P = *(line + (x * dx));
		  if (P < 0) {
		    ENCODE_BIT (NEGATIVE_BIT);
		    P *= -1;
		  } else {
		    ENCODE_BIT (POSITIVE_BIT);
		  }
		  BINARY_ENCODE ( P + 1, maxval + 1);	/* |P****** */
		  continue;	/* |******* */
		}
	    }

	  if (H < 0) {
	    H *= -1;
	  }
	  if (L < 0) {
	    L *= -1;
	  }

	  H = N1 > N2 ? N1 : N2;
	  L = N1 < N2 ? N1 : N2;
	  delta = H - L;

	  // line[x] = P = get (fp_in);
	  P = *(line + (x*dx));
	  if (P < 0) {
	    ENCODE_BIT (NEGATIVE_BIT);
	    P *= -1;
	  } else {
	    ENCODE_BIT (POSITIVE_BIT);
	  }

	  if ((L <= P) && (P <= H))
	    {
	      /* IN-RANGE */
	      long range, logofrange, thresh, numlong;
	      int nbits;
	      
	      ENCODE_BIT (IN_RANGE);

	      P -= L;

	      range = delta + 1;

	      if (range > 1)
		{
		  /* 2**i <= delta + 1 < 2**(i+1) ; find i ... */
		  CEILLOG_2 (range, logofrange);

		  /* number of n-bit values in the range */
		  thresh = (1 << logofrange) - range;

		  /* number of n+1-bit values in the range */
		  numlong = range - thresh;

		  /* rotate the n-bit codes into the centre */
		  P -= (numlong >> 1);
		  if ((long) P < 0)
		    P += range;

		  if (P < thresh)
		    nbits = logofrange - 1;
		  else
		    {
		      nbits = logofrange;
		      P += thresh;
		    }
		  while (--nbits >= 0)
		    ENCODE_BIT ((P >> nbits) & 1);
		}
	    }
	  else
	    {
	      /* OUT_OF_RANGE */
	      unsigned long diff, parm, i, min;

	      ENCODE_BIT (OUT_OF_RANGE);

	      if (P < L)
		{
		  ENCODE_BIT (BELOW_RANGE);
		  diff = L - P - 1;
		}
	      else
		{
		  ENCODE_BIT (ABOVE_RANGE);
		  diff = P - H - 1;
		}

	      /* Now code 'diff' as a Golomb-Rice code */

	      parm = 0;
	      cumul_param_ptr = (goUInt32*)cumul_param[delta];
	      if (cumul_param_ptr == 0) {
		// These are not deleted yet. Memory leak.
		cumul_param_ptr = new goUInt32[INTSIZE];
		memset (cumul_param_ptr, 0, INTSIZE * sizeof (goUInt32));
		cumul_param.add(delta,(void*)cumul_param_ptr);
	      }
	      min = cumul_param_ptr[0];
	      for (i = 1; i < max_cumul; i++)
		if (cumul_param_ptr[i] < min)
		  {
		    min = cumul_param_ptr[i];
		    parm = i;
		  }
	      //	      parm = 6;
	      UNARY_ENCODE ((diff >> parm) + 1);
	      BINARY_ENCODE ((diff & ((1 << parm) - 1)) + 1, 1 << parm);

  	      for (i = 0; i < max_cumul; i++)
  		cumul_param_ptr[i] += (diff >> i) + 1 + i;
	    }
	  
	}
    }
  }
  ENCODE_DONE

}



template< class T >
void 
goFELICSdecode (goArray<T> *array, goSize_t startIdx,
		T maxval, int maxk,
		go3DBlock<T> *block)
{
  goSize_t x, y, z;
  long P, L = 0, H = 0, N1, N2, delta;
  goHashTable<goUInt32,void*> cumul_param;
  goUInt32* cumul_param_ptr = 0;
  // unsigned long cumul_param[4096][INTSIZE];
  unsigned max_cumul;
  bool negativeFlag = false;

  // memset (cumul_param, '0', sizeof (cumul_param));

  if (maxk)
    max_cumul = maxk;
  else
    CEILLOG_2 (maxval, max_cumul);

  DECODE_START (array,startIdx)

    T *line;
  goPtrdiff_t dx = block->getXDiff();
  goPtrdiff_t dy = block->getYDiff();
  for (z = 0; z < block->getSizeZ(); z++) {
    // cout << "z = " << z << endl;
    for (y = 0; y < block->getSizeY(); y++) {
      line = block->getPtr (0,y,z);
      for (x = 0; x < block->getSizeX(); x++)
	  {
	    if (y > 0)
	      {
		if (x > 0)
		  {			/* ******** */
		    N1 = *(line + (x * dx) - dy);	/* ****1*** */
		    N2 = *(line + ((x - 1) * dx));	/* ***2P*** */
		  }			/* ******** */
		else
		  {			/* |******* */
		    N1 = *(line + (x * dx) - dy);	/* |12***** */
		    N2 = *(line + ((x + 1) * dx) - dy );	/* |P***** */
		  }			/* |******* */
	      }
	    else
	      {
		if (x > 1)
		  {
		    N1 = *(line + ((x - 1) * dx));	/* -------- */
		    N2 = *(line + ((x - 2) * dx));	/* **12P*** */
		  }			/* ******** */
		else if (x > 0)
		  {
		    N1 = *(line + ((x - 1) * dx));	/* +------- */
		    N2 = N1;	/* |3P***** */
		  }			/* |******* */
		else
		  {
		    // cout << "Binary decoding " << x << "," << y << "," << z;
		    if (DECODE_BIT == NEGATIVE_BIT) {
		      negativeFlag = true;
		    } else {
		      negativeFlag = false;
		    }
		    BINARY_DECODE ( P, maxval + 1);
		    P--;
		    if (negativeFlag) {
		      P *= -1;
		    }
		    *(line + (x * dx)) = (T)P;
		    // cout << " = " << *(line + (x * dx)) << endl;
		    //if (maxval < 256)
		    //  putc (line[x], fp_out);	/* +------- */
		    //else		/* |P****** */
		    // fprintf (fp_out, "%u ", line[x]);	/* |******* */
		    
		    continue;
		  }
	      }

	    if (H < 0) {
	      H *= -1;
	    }
	    if (L < 0) {
	      L *= -1;
	    }
	    
	    H = N1 > N2 ? N1 : N2;
	    L = N1 < N2 ? N1 : N2;
	    // H = L = 0;
	    delta = H - L;

	    if (DECODE_BIT == NEGATIVE_BIT) {
	      negativeFlag = true;
	    } else {
	      negativeFlag = false;
	    }

	    if (DECODE_BIT == IN_RANGE)
	      {
		// unsigned long i, range, logofrange, thresh, numlong;
		long i, range, logofrange, thresh, numlong;
		unsigned xx, l;

		range = delta + 1;

		if (range > 1)
		  {
		    /* 2**i <= delta + 1 < 2**(i+1) ; find i ... */
		    CEILLOG_2 (range, logofrange);

		    /* number of n-bit values in the range */
		    thresh = (1 << logofrange) - range;
		    for (P = i = 0; i < logofrange - 1; i++)
		      DECODE_ADD (P);
		    xx = P;
		    l = logofrange - 1;
		    if (P >= thresh)
		      {
			DECODE_ADD (P);
			xx = P;
			l++;
			P -= thresh;
		      }

		    /* number of n+1-bit values in the range */
		    numlong = range - thresh;

		    /* rotate the n-bit codes into the centre */
		    P += (numlong >> 1);
		    if ( P >= range)
		      P -= range;

		    P += L;
		  }
		else
		  P = L;
	      }
	    else
	      {
		unsigned int out_of_range = DECODE_BIT;
		unsigned long diff, unary, binary, i, min, parm;

		/* Now code 'diff' as a Golomb-Rice code */

		parm = 0;
		cumul_param_ptr = (goUInt32*)cumul_param[delta];
		if (cumul_param_ptr == 0) {
		  // These are not deleted yet. Memory leak.
		  cumul_param_ptr = new goUInt32[INTSIZE];
		  memset (cumul_param_ptr, 0, INTSIZE * sizeof (goUInt32));
		  cumul_param.add(delta, (void*)cumul_param_ptr);
		}
		min = cumul_param_ptr[0];
		for (i = 1; i < max_cumul; i++) {
		  if (cumul_param_ptr[i] < min)
		    {
		      min = cumul_param_ptr[i];
		      parm = i;
		    }
		}
		
		//		parm = 6;
		UNARY_DECODE (unary);
		diff = (unary - 1) << parm;
		BINARY_DECODE (binary, 1 << parm);
		diff |= binary - 1;

  		for (i = 0; i < max_cumul; i++)
  		  cumul_param_ptr[i] += (diff >> i) + 1 + i;

		if (out_of_range == BELOW_RANGE)
		  {
		    P = L - diff - 1;
		  }
		else
		  {
		    P = diff + H + 1;
		  }
	      }

	    if (negativeFlag) {
	      P *= -1;
	    }
	    *(line + (x * dx)) = (T)P;
			    
	    //if (maxval < 256)
	    //  (void) putc (P, fp_out);
	    //else
	    //  fprintf (fp_out, "%u ", P);
	  }
      }
    }
  DECODE_DONE
}

template void goFELICSencode<goInt16> (go3DBlock<goInt16> *block, 
				       goInt16 maxval, int maxk,
				       goArray<goInt16> *array);

template void goFELICSdecode<goInt16> (goArray<goInt16> *array, goSize_t startIdx,
				       goInt16 maxval, int maxk,
				       go3DBlock<goInt16> *block);

template void goFELICSencode<goInt32> (go3DBlock<goInt32> *block, 
				       int maxval, int maxk,
				       goArray<goInt32> *array);

template void goFELICSdecode<goInt32> (goArray<goInt32> *array, goSize_t startIdx,
				       int maxval, int maxk,
				       go3DBlock<goInt32> *block);


